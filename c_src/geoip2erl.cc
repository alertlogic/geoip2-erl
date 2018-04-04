#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <endian.h>

#include <sstream>
#include <stdexcept>
#include <stack>

#include <ei.h>
#include <maxminddb.h>

using namespace std;

struct end_of_stream : std::exception {
    virtual const char* what() const throw () { return "end of stream"; }
};

#define STDIN  0
#define STDOUT 1

int read_packet_size(size_t maxsize) {
    uint32_t size;
    int rc = read(STDIN, &size, 4);
    if (rc == 0) {
        throw end_of_stream();
    }

    if (rc != 4)
        throw std::runtime_error("missing packet length");
       
    size = be32toh(size);
    if (size > maxsize)
        throw std::runtime_error("packet length exceeds maximum message size ");

    return size;
}

int read_packet(unsigned char* buffer, size_t size) {
    if (size == 0)
        return size;

    size_t offset = 0;
    while( offset < size ) {
        ssize_t rd = read(STDIN, buffer + offset, size - offset);
        if (rd < 0)
            throw std::runtime_error("read error");
        if (rd == 0)
            throw std::runtime_error("unexpected end of stream");
        offset += rd;
    }

    return size;
}

void write_all(const unsigned char* buffer, size_t count) {
    size_t offset = 0;
    while( offset < count ) {
        ssize_t wr = write(STDOUT, buffer + offset, count - offset);
        if (wr <= 0)
            throw std::runtime_error("write error");
        offset += wr;
    }
}

int receive(unsigned char* buffer, size_t maxsize) {
    size_t size = read_packet_size(maxsize);
    return read_packet(buffer, size);
}

void send(const unsigned char* buffer, size_t size) {
    uint32_t size32 = htobe32(size);
    if (write(STDOUT, &size32, 4) != 4) 
        throw std::runtime_error("write error");
    write_all(buffer, size);
}

void send(const ei_x_buff* out) {
    send((const unsigned char*)out->buff, out->index);
}

void send_error(const char* msg) {
    ei_x_buff out;
    ei_x_new_with_version(&out);
    ei_x_encode_tuple_header(&out, 2);
    ei_x_encode_atom(&out, "error");
    ei_x_encode_string(&out, msg);
    send(&out);
    ei_x_free(&out);
}

int main() {
    ei_x_buff     out;
    unsigned char in[4096];
    int in_s;

    in_s = receive(in, sizeof(in) - 1);
    in[in_s] = 0;

    MMDB_s mmdb;
    int status = MMDB_open((const char*)in, MMDB_MODE_MMAP, &mmdb);

    if (MMDB_SUCCESS != status) {
        stringstream ss;
        ss << (const char*)in << ": " << MMDB_strerror(status);
        send_error(ss.str().c_str());
        exit(1);
    }

    ei_x_new_with_version(&out);
    ei_x_encode_atom(&out, "ok");
    send(&out);
    ei_x_free(&out);

    int gai_error;
    MMDB_lookup_result_s result;

    try {
        while(1) {
            in_s = receive(in, sizeof(in) - 1);
            in[in_s] = 0;

            result = MMDB_lookup_string(&mmdb, (const char*)in, &gai_error, &status);

            if (0 != gai_error) {
                send_error(gai_strerror(gai_error));
                continue;
            }

            if (MMDB_SUCCESS != status) {
                send_error(MMDB_strerror(status));
                continue;
            }

            if (!result.found_entry) {
                send_error("not found");
                continue;
            }

            MMDB_entry_data_list_s *entry_data_list = NULL;
            status = MMDB_get_entry_data_list(&result.entry, &entry_data_list);
            if (MMDB_SUCCESS != status) {
                send_error(MMDB_strerror(status));
                continue;
            }

            ei_x_new_with_version(&out);
            ei_x_encode_tuple_header(&out, 2);
            ei_x_encode_atom(&out, "ok");

            MMDB_entry_data_list_s* current = entry_data_list;
            stack< pair<bool,int> > maps_arrays;
            while(current) {
                MMDB_entry_data_s& data = current->entry_data;

                switch(data.type) {
                case MMDB_DATA_TYPE_UTF8_STRING: 
                    ei_x_encode_binary(&out, data.utf8_string, data.data_size);
                    break;
                case MMDB_DATA_TYPE_DOUBLE:
                    ei_x_encode_double(&out, data.double_value);
                    break;
                case MMDB_DATA_TYPE_BYTES:
                    ei_x_encode_binary(&out, data.bytes, data.data_size);
                    break;
                case MMDB_DATA_TYPE_UINT16:
                    ei_x_encode_ulong(&out, data.uint16);
                    break;
                case MMDB_DATA_TYPE_UINT32:
                    ei_x_encode_ulong(&out, data.uint32);
                    break;
                case MMDB_DATA_TYPE_MAP:
                    ei_x_encode_map_header(&out, data.data_size);
                    maps_arrays.push(make_pair(false, data.data_size * 2));
                    break;
                case MMDB_DATA_TYPE_INT32:
                    ei_x_encode_long(&out, data.int32);
                    break;
                case MMDB_DATA_TYPE_UINT64:
                    ei_x_encode_ulonglong(&out, data.uint64);
                    break;
                case MMDB_DATA_TYPE_UINT128:
                    ei_x_encode_ulonglong(&out, data.uint128);
                    break;
                case MMDB_DATA_TYPE_ARRAY:
                    ei_x_encode_list_header(&out, data.data_size);
                    maps_arrays.push(make_pair(true, data.data_size));
                    break;
                case MMDB_DATA_TYPE_BOOLEAN:
                    ei_x_encode_atom(&out, data.boolean ? "true" : "false");
                    break;
                case MMDB_DATA_TYPE_FLOAT:
                    ei_x_encode_double(&out, data.float_value);
                    break;
                default:
                    stringstream ss;
                    ss << "Unexpected data type: " << data.type;
                    throw ss.str();
                }

                if (data.type != MMDB_DATA_TYPE_MAP && data.type != MMDB_DATA_TYPE_ARRAY) {
                    while(!maps_arrays.empty()) {
                        pair<bool,int>& top = maps_arrays.top();
                        if (--top.second <= 0) {
                            if (top.first) {
                                ei_x_encode_empty_list(&out);
                            }
                            maps_arrays.pop();
                        } else {
                            break;
                        }
                    }
                }

                current = current->next;
            }
            MMDB_free_entry_data_list(entry_data_list);

            send(&out);
            ei_x_free(&out);
        }
    } catch(const end_of_stream& eof) {
        // ok
    }

    MMDB_close(&mmdb);
    
    return 0;
}
