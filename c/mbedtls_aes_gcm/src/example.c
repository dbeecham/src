#define _DEFAULT_SOURCE

#include <fcntl.h>
#include <unistd.h>
#include <syslog.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>

#include <mbedtls/gcm.h>
#include <mbedtls/error.h>


// note: 
//   * mbedtls_gcm_selftest is useful to call as well
//   * mbedtls_gcm_crypt_and_tag does the encrypting in one step
//   * mbedtls_gcm_auth_decrypt does the decrypting in one step

int example_encrypt (
    void
)
{
    int ret = 0;
    int outfile_len = 0;
    mbedtls_gcm_context gcm;

    mbedtls_gcm_init(&gcm);
    const char key[32] = "shiet1biethaL0aloh8ooRahxi7faedo";
    ret = mbedtls_gcm_setkey(
        /* gcm = */ &gcm,
        /* cipher = */ MBEDTLS_CIPHER_ID_AES, 
        /* key = */ (const unsigned char *)key,
        /* key_len (128, 192, or 256) = */ 256
    );
    if (0 != ret) {
        char error[512];
        mbedtls_strerror(ret, error, 512);
        syslog(LOG_ERR, "%s:%d:%s: mbedtls_gcm_setkey returned %d: %.*s", __FILE__, __LINE__, __func__, ret, 512, error);
        return -1;
    }

    const char iv[32] = "nahmuwaevoob7Ouz5eiK8uu2Phuinai4";
    ret = mbedtls_gcm_starts(
        /* gcm = */ &gcm,
        /* cipher = */ MBEDTLS_GCM_ENCRYPT,
        /* iv = */ (const unsigned char *)iv,
        /* iv_len = */ sizeof(iv),
        /* additional = */ NULL,
        /* additional_len = */ 0
    );
    if (0 != ret) {
        char error[512];
        mbedtls_strerror(ret, error, sizeof(error));
        syslog(LOG_ERR, "%s:%d:%s: mbedtls_gcm_starts returned %d: %.*s", __FILE__, __LINE__, __func__, ret, 512, error);
        return -1;
    }

    int fd = open(
        /* path  = */ "out.enc",
        /* flags = */ O_RDWR | O_CLOEXEC | O_CREAT,
        /* mode = */ S_IRUSR | S_IWUSR
    );
    if (-1 == fd) {
        syslog(LOG_ERR, "%s:%d:%s: open: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    // both out and buf must be at least buf_len in size. mbedtls_gcm_update
    // expects buf_len to be a multiple of 16 in all but the last call before
    // mbedtls_gcm_finish
    unsigned char buf[] = "Hello, world.";
    unsigned char out[512];
    ret = mbedtls_gcm_update(
        /* gcm = */ &gcm,
        /* buf_len = */ strlen((char*)buf),
        /* buf = */ buf,
        /* out = */ out
    );
    if (0 != ret) {
        char error[512];
        mbedtls_strerror(ret, error, sizeof(error));
        syslog(LOG_ERR, "%s:%d:%s: mbedtls_gcm_update returned %d: %.*s", __FILE__, __LINE__, __func__, ret, 512, error);
        return -1;
    }

    int bytes_written = write(fd, out, strlen((char*)buf));
    if (-1 == bytes_written) {
        syslog(LOG_ERR, "%s:%d:%s: write: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    outfile_len += bytes_written;


    // finishes GCM operations and generates an authentication tag (MAC).
    // tag_len must be at least 4.
    unsigned char tag[16];
    ret = mbedtls_gcm_finish(
        /* gcm = */ &gcm,
        /* tag = */ tag,
        /* tag_len = */ sizeof(tag)
    );
    if (0 != ret) {
        char error[512];
        mbedtls_strerror(ret, error, sizeof(error));
        syslog(LOG_ERR, "%s:%d:%s: mbedtls_gcm_finish returned %d: %.*s", __FILE__, __LINE__, __func__, ret, 512, error);
        return -1;
    }

    // write the tag to file as well
    bytes_written = write(fd, tag, 16);
    if (-1 == bytes_written) {
        syslog(LOG_ERR, "%s:%d:%s: write: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    outfile_len += bytes_written;

    ret = ftruncate(fd, outfile_len);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: ftruncate: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    mbedtls_gcm_free(&gcm);
    
    close(fd);
    fd = -1;

    return 0;
}


int example_decrypt (
    void
)
{
    int ret = 0;
    int bytes_read = 0;
    mbedtls_gcm_context gcm;

    mbedtls_gcm_init(&gcm);
    const char key[32] = "shiet1biethaL0aloh8ooRahxi7faedo";
    ret = mbedtls_gcm_setkey(
        /* gcm = */ &gcm,
        /* cipher = */ MBEDTLS_CIPHER_ID_AES, 
        /* key = */ (const unsigned char *)key,
        /* key_len (128, 192, or 256) = */ 256
    );
    if (0 != ret) {
        char error[512];
        mbedtls_strerror(ret, error, 512);
        syslog(LOG_ERR, "%s:%d:%s: mbedtls_gcm_setkey returned %d: %.*s", __FILE__, __LINE__, __func__, ret, 512, error);
        return -1;
    }

    const char iv[32] = "nahmuwaevoob7Ouz5eiK8uu2Phuinai4";
    ret = mbedtls_gcm_starts(
        /* gcm = */ &gcm,
        /* cipher = */ MBEDTLS_GCM_DECRYPT,
        /* iv = */ (const unsigned char *)iv,
        /* iv_len = */ sizeof(iv),
        /* additional = */ NULL,
        /* additional_len = */ 0
    );
    if (0 != ret) {
        char error[512];
        mbedtls_strerror(ret, error, sizeof(error));
        syslog(LOG_ERR, "%s:%d:%s: mbedtls_gcm_starts returned %d: %.*s", __FILE__, __LINE__, __func__, ret, 512, error);
        return -1;
    }

    int fd = open(
        /* path  = */ "out.enc",
        /* flags = */ O_RDONLY | O_CLOEXEC
    );
    if (-1 == fd) {
        syslog(LOG_ERR, "%s:%d:%s: open: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    unsigned char buf[1024];
    bytes_read = read(fd, buf, sizeof(buf));
    if (-1 == bytes_read) {
        syslog(LOG_ERR, "%s:%d:%s: read: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    // both out and buf must be at least buf_len in size. mbedtls_gcm_update
    // expects buf_len to be a multiple of 16 in all but the last call before
    // mbedtls_gcm_finish
    unsigned char out[512];
    ret = mbedtls_gcm_update(
        /* gcm = */ &gcm,
        /* buf_len = */ bytes_read - 16, // the last 16 bytes is the MAC
        /* buf = */ buf,
        /* out = */ out
    );
    if (0 != ret) {
        char error[512];
        mbedtls_strerror(ret, error, sizeof(error));
        syslog(LOG_ERR, "%s:%d:%s: mbedtls_gcm_update returned %d: %.*s", __FILE__, __LINE__, __func__, ret, 512, error);
        return -1;
    }

    // finishes GCM operations and generates an authentication tag (MAC).
    // tag_len must be at least 4, and max 16.
    unsigned char tag[16];
    ret = mbedtls_gcm_finish(
        /* gcm = */ &gcm,
        /* tag = */ tag,
        /* tag_len = */ sizeof(tag)
    );
    if (0 != ret) {
        char error[512];
        mbedtls_strerror(ret, error, sizeof(error));
        syslog(LOG_ERR, "%s:%d:%s: mbedtls_gcm_finish returned %d: %.*s", __FILE__, __LINE__, __func__, ret, 512, error);
        return -1;
    }

    for (int i = 0; i < 16; i++) {
        if (buf[bytes_read - 16 + i] != tag[i]) {
            syslog(LOG_ERR, "%s:%d:%s: tag mismatch at index %d", __FILE__, __LINE__, __func__, i);
            return -1;
        }
    }

    mbedtls_gcm_free(&gcm);

    close(fd);
    fd = -1;

    printf("%s:%d:%s: decoded: %.*s\n", __FILE__, __LINE__, __func__, bytes_read - 16, out);

    return 0;
}


int example_encrypt_with_additional (
    void
)
{
    int ret = 0;
    int outfile_len = 0;
    mbedtls_gcm_context gcm;

    mbedtls_gcm_init(&gcm);
    const char key[32] = "shiet1biethaL0aloh8ooRahxi7faedo";
    ret = mbedtls_gcm_setkey(
        /* gcm = */ &gcm,
        /* cipher = */ MBEDTLS_CIPHER_ID_AES, 
        /* key = */ (const unsigned char *)key,
        /* key_len (128, 192, or 256) = */ 256
    );
    if (0 != ret) {
        char error[512];
        mbedtls_strerror(ret, error, 512);
        syslog(LOG_ERR, "%s:%d:%s: mbedtls_gcm_setkey returned %d: %.*s", __FILE__, __LINE__, __func__, ret, 512, error);
        return -1;
    }

    const char iv[32] = "nahmuwaevoob7Ouz5eiK8uu2Phuinai4";
    ret = mbedtls_gcm_starts(
        /* gcm = */ &gcm,
        /* cipher = */ MBEDTLS_GCM_ENCRYPT,
        /* iv = */ (const unsigned char *)iv,
        /* iv_len = */ sizeof(iv),
        /* additional = */ (const unsigned char*)"my_additional_data",
        /* additional_len = */ strlen("my_additional_data")
    );
    if (0 != ret) {
        char error[512];
        mbedtls_strerror(ret, error, sizeof(error));
        syslog(LOG_ERR, "%s:%d:%s: mbedtls_gcm_starts returned %d: %.*s", __FILE__, __LINE__, __func__, ret, 512, error);
        return -1;
    }

    int fd = open(
        /* path  = */ "out-additional.enc",
        /* flags = */ O_RDWR | O_CLOEXEC | O_CREAT,
        /* mode = */ S_IRUSR | S_IWUSR
    );
    if (-1 == fd) {
        syslog(LOG_ERR, "%s:%d:%s: open: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    // both out and buf must be at least buf_len in size. mbedtls_gcm_update
    // expects buf_len to be a multiple of 16 in all but the last call before
    // mbedtls_gcm_finish
    unsigned char buf[] = "Hello, world.";
    unsigned char out[512];
    ret = mbedtls_gcm_update(
        /* gcm = */ &gcm,
        /* buf_len = */ strlen((char*)buf),
        /* buf = */ buf,
        /* out = */ out
    );
    if (0 != ret) {
        char error[512];
        mbedtls_strerror(ret, error, sizeof(error));
        syslog(LOG_ERR, "%s:%d:%s: mbedtls_gcm_update returned %d: %.*s", __FILE__, __LINE__, __func__, ret, 512, error);
        return -1;
    }

    int bytes_written = write(fd, out, strlen((char*)buf));
    if (-1 == bytes_written) {
        syslog(LOG_ERR, "%s:%d:%s: write: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    outfile_len += bytes_written;


    // finishes GCM operations and generates an authentication tag (MAC).
    // tag_len must be at least 4.
    unsigned char tag[16];
    ret = mbedtls_gcm_finish(
        /* gcm = */ &gcm,
        /* tag = */ tag,
        /* tag_len = */ sizeof(tag)
    );
    if (0 != ret) {
        char error[512];
        mbedtls_strerror(ret, error, sizeof(error));
        syslog(LOG_ERR, "%s:%d:%s: mbedtls_gcm_finish returned %d: %.*s", __FILE__, __LINE__, __func__, ret, 512, error);
        return -1;
    }

    // write the tag to file as well
    bytes_written = write(fd, tag, 16);
    if (-1 == bytes_written) {
        syslog(LOG_ERR, "%s:%d:%s: write: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    outfile_len += bytes_written;

    ret = ftruncate(fd, outfile_len);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: ftruncate: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    mbedtls_gcm_free(&gcm);
    
    close(fd);
    fd = -1;

    return 0;
}

int example_decrypt_with_additional (
    void
)
{
    int ret = 0;
    int bytes_read = 0;
    mbedtls_gcm_context gcm;

    mbedtls_gcm_init(&gcm);
    const char key[32] = "shiet1biethaL0aloh8ooRahxi7faedo";
    ret = mbedtls_gcm_setkey(
        /* gcm = */ &gcm,
        /* cipher = */ MBEDTLS_CIPHER_ID_AES, 
        /* key = */ (const unsigned char *)key,
        /* key_len (128, 192, or 256) = */ 256
    );
    if (0 != ret) {
        char error[512];
        mbedtls_strerror(ret, error, 512);
        syslog(LOG_ERR, "%s:%d:%s: mbedtls_gcm_setkey returned %d: %.*s", __FILE__, __LINE__, __func__, ret, 512, error);
        return -1;
    }

    const char iv[32] = "nahmuwaevoob7Ouz5eiK8uu2Phuinai4";
    ret = mbedtls_gcm_starts(
        /* gcm = */ &gcm,
        /* cipher = */ MBEDTLS_GCM_DECRYPT,
        /* iv = */ (const unsigned char *)iv,
        /* iv_len = */ sizeof(iv),
        /* additional = */ (const unsigned char*)"my_additional_data",
        /* additional_len = */ strlen("my_additional_data")
    );
    if (0 != ret) {
        char error[512];
        mbedtls_strerror(ret, error, sizeof(error));
        syslog(LOG_ERR, "%s:%d:%s: mbedtls_gcm_starts returned %d: %.*s", __FILE__, __LINE__, __func__, ret, 512, error);
        return -1;
    }

    int fd = open(
        /* path  = */ "out-additional.enc",
        /* flags = */ O_RDONLY | O_CLOEXEC
    );
    if (-1 == fd) {
        syslog(LOG_ERR, "%s:%d:%s: open: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    unsigned char buf[1024];
    bytes_read = read(fd, buf, sizeof(buf));
    if (-1 == bytes_read) {
        syslog(LOG_ERR, "%s:%d:%s: read: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    // both out and buf must be at least buf_len in size. mbedtls_gcm_update
    // expects buf_len to be a multiple of 16 in all but the last call before
    // mbedtls_gcm_finish
    unsigned char out[512];
    ret = mbedtls_gcm_update(
        /* gcm = */ &gcm,
        /* buf_len = */ bytes_read - 16, // the last 16 bytes is the MAC
        /* buf = */ buf,
        /* out = */ out
    );
    if (0 != ret) {
        char error[512];
        mbedtls_strerror(ret, error, sizeof(error));
        syslog(LOG_ERR, "%s:%d:%s: mbedtls_gcm_update returned %d: %.*s", __FILE__, __LINE__, __func__, ret, 512, error);
        return -1;
    }

    // finishes GCM operations and generates an authentication tag (MAC).
    // tag_len must be at least 4, and max 16.
    unsigned char tag[16];
    ret = mbedtls_gcm_finish(
        /* gcm = */ &gcm,
        /* tag = */ tag,
        /* tag_len = */ sizeof(tag)
    );
    if (0 != ret) {
        char error[512];
        mbedtls_strerror(ret, error, sizeof(error));
        syslog(LOG_ERR, "%s:%d:%s: mbedtls_gcm_finish returned %d: %.*s", __FILE__, __LINE__, __func__, ret, 512, error);
        return -1;
    }

    for (int i = 0; i < 16; i++) {
        if (buf[bytes_read - 16 + i] != tag[i]) {
            syslog(LOG_ERR, "%s:%d:%s: tag mismatch at index %d", __FILE__, __LINE__, __func__, i);
            return -1;
        }
    }

    mbedtls_gcm_free(&gcm);

    close(fd);
    fd = -1;

    printf("%s:%d:%s: decoded: %.*s\n", __FILE__, __LINE__, __func__, bytes_read - 16, out);

    return 0;
}


int main (
    int argc,
    char const* argv[]
)
{
    int ret = 0;

    ret = example_encrypt();
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: example_encrypt returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }

    ret = example_decrypt();
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: example_decrypt returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }


    ret = example_encrypt_with_additional();
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: example_encrypt returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }

    ret = example_decrypt_with_additional();
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: example_encrypt returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }

    openlog("example", LOG_CONS | LOG_PID, LOG_USER);

}
