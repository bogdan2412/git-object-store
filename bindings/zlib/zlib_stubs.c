/* zlib bindings for OCaml.

   Copyright (C) 2019-2022  Bogdan-Cristian Tataroiu

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>. */

#include <errno.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/threads.h>

#include <zlib.h>

enum state {
  NOT_INITIALISED = 0,
  INITIALISED_INFLATE = 1,
  INITIALISED_DEFLATE = 2
};

struct wrap_stream {
  z_stream *z_stream;
  enum state state;
};

#define Zstream_val(v) (((struct wrap_stream *)(Data_custom_val(v)))->z_stream)
#define State(v) (((struct wrap_stream *)(Data_custom_val(v)))->state)

static value *zlib_error_exn = NULL;

static void git_zlib_error(char *fn, value zstream, int ret_code) {
  if (ret_code == Z_OK) {
    return;
  }

  char *msg;
  value s1 = Val_unit, s2 = Val_unit, bucket = Val_unit;

  if (ret_code == Z_ERRNO) {
    msg = strerror(errno);
  } else {
    msg = Zstream_val(zstream)->msg;
    if (msg == NULL) {
      msg = "";
    }
  }
  if (zlib_error_exn == NULL) {
    zlib_error_exn = caml_named_value("Zlib.Error");
    if (zlib_error_exn == NULL) {
      caml_invalid_argument("Exception Zlib.Error not initialized");
    }
  }
  Begin_roots3(s1, s2, bucket);
  s1 = copy_string(fn);
  s2 = copy_string(msg);
  bucket = caml_alloc_small(4, 0);
  Field(bucket, 0) = *zlib_error_exn;
  Field(bucket, 1) = s1;
  Field(bucket, 2) = s2;
  Field(bucket, 3) = Val_int(ret_code);
  End_roots();
  caml_raise(bucket);
}

static value git_zlib_reset_zstream(z_stream *zstream) {
  zstream->next_in = NULL;
  zstream->avail_in = 0;
  zstream->total_in = 0;

  zstream->next_out = NULL;
  zstream->avail_out = 0;
  zstream->total_out = 0;

  zstream->msg = NULL;

  zstream->zalloc = NULL;
  zstream->zfree = NULL;
  zstream->opaque = NULL;
  return Val_unit;
}

value git_zlib_avail_in(value zstream) {
  return Val_int(Zstream_val(zstream)->avail_in);
}

value git_zlib_avail_out(value zstream) {
  return Val_int(Zstream_val(zstream)->avail_out);
}

static void git_zlib_free_zstream_internal(value wrap_stream) {
  z_stream *zstream = Zstream_val(wrap_stream);
  int ret_code;

  switch (State(wrap_stream)) {
  case NOT_INITIALISED:
    break;
  case INITIALISED_INFLATE:
    ret_code = inflateEnd(zstream);
    git_zlib_error("git_zlib_free_zstream_internal", wrap_stream,
                   ret_code == Z_DATA_ERROR ? 0 : ret_code);
    State(wrap_stream) = NOT_INITIALISED;
    break;
  case INITIALISED_DEFLATE:
    ret_code = deflateEnd(zstream);
    git_zlib_error("git_zlib_free_zstream_internal", wrap_stream,
                   ret_code == Z_DATA_ERROR ? 0 : ret_code);
    State(wrap_stream) = NOT_INITIALISED;
    break;
  }

  git_zlib_reset_zstream(zstream);
}

static void git_zlib_free(value zstream) {
  git_zlib_free_zstream_internal(zstream);
  caml_stat_free(Zstream_val(zstream));
}

value git_zlib_create_uninitialised_zstream(void) {
  value ret;
  ret = caml_alloc_final((sizeof(struct wrap_stream) + sizeof(value) - 1) /
                             sizeof(value),
                         git_zlib_free, 1 << MAX_WBITS, 1 << 20);
  Zstream_val(ret) = caml_stat_alloc(sizeof(z_stream));
  State(ret) = NOT_INITIALISED;
  return ret;
}

value git_zlib_inflate_init(value zstream) {
  git_zlib_free_zstream_internal(zstream);
  git_zlib_error("Zlib.Inflate.init", zstream,
                 inflateInit(Zstream_val(zstream)));
  State(zstream) = INITIALISED_INFLATE;
  return Val_unit;
}

value git_zlib_inflate_reset(value zstream) {
  git_zlib_error("Zlib.Inflate.reset", zstream,
                 inflateReset(Zstream_val(zstream)));
  return Val_unit;
}

value git_zlib_inflate_process(value zstream, value src, value src_pos,
                               value src_len, value dst, value dst_pos,
                               value dst_len, value finish) {
  z_stream *zs = Zstream_val(zstream);
  int ret_code;

  zs->next_in = &Byte_u(Caml_ba_data_val(src), Long_val(src_pos));
  zs->avail_in = Long_val(src_len);
  zs->next_out = &Byte_u(Caml_ba_data_val(dst), Long_val(dst_pos));
  zs->avail_out = Long_val(dst_len);
  caml_release_runtime_system();
  ret_code = inflate(zs, Bool_val(finish) ? Z_FINISH : Z_NO_FLUSH);
  caml_acquire_runtime_system();
  if (ret_code == Z_OK || ret_code == Z_BUF_ERROR || ret_code == Z_STREAM_END) {
    return Val_bool(ret_code == Z_STREAM_END);
  } else {
    git_zlib_error("Zlib.Inflate.process", zstream, ret_code);
  }
}

value git_zlib_inflate_process_bytecode(value *arg, int nargs) {
  return git_zlib_inflate_process(arg[0], arg[1], arg[2], arg[3], arg[4],
                                  arg[5], arg[6], arg[7]);
}

value git_zlib_deflate_init(value zstream) {
  git_zlib_free_zstream_internal(zstream);
  git_zlib_error("Zlib.Deflate.init", zstream,
                 deflateInit(Zstream_val(zstream), Z_DEFAULT_COMPRESSION));
  State(zstream) = INITIALISED_DEFLATE;
  return Val_unit;
}

value git_zlib_deflate_reset(value zstream) {
  git_zlib_error("Zlib.Deflate.reset", zstream,
                 deflateReset(Zstream_val(zstream)));
  return Val_unit;
}

value git_zlib_deflate_process(value zstream, value src, value src_pos,
                               value src_len, value dst, value dst_pos,
                               value dst_len, value finish) {
  z_stream *zs = Zstream_val(zstream);
  int ret_code;

  zs->next_in = &Byte_u(Caml_ba_data_val(src), Long_val(src_pos));
  zs->avail_in = Long_val(src_len);
  zs->next_out = &Byte_u(Caml_ba_data_val(dst), Long_val(dst_pos));
  zs->avail_out = Long_val(dst_len);
  caml_release_runtime_system();
  ret_code = deflate(zs, Bool_val(finish) ? Z_FINISH : Z_NO_FLUSH);
  caml_acquire_runtime_system();
  if (ret_code == Z_OK || ret_code == Z_BUF_ERROR || ret_code == Z_STREAM_END) {
    return Val_bool(ret_code == Z_STREAM_END);
  } else {
    git_zlib_error("Zlib.Deflate.process", zstream, ret_code);
  }
}

value git_zlib_deflate_process_bytecode(value *arg, int nargs) {
  return git_zlib_deflate_process(arg[0], arg[1], arg[2], arg[3], arg[4],
                                  arg[5], arg[6], arg[7]);
}
