library(diffrprojects)

x <- 1:(10)^7
res_x <-
  microbenchmark::microbenchmark(
    digest::digest(x, "md5"     ),
    digest::digest(x, "sha1"    ),
    digest::digest(x, "crc32"   ),
    digest::digest(x, "sha256"  ),
    digest::digest(x, "sha512"  ),
    digest::digest(x, "xxhash32"),
    digest::digest(x, "xxhash64"),
    digest::digest(x, "murmur32"),
    dp_hash(x),
    times = 10
  )

res_x


y <- list()
for (i in 1:100) {
  y[[digest::digest(i)]] <- data.frame(
    a = 1:1000, b = sample(letters, 1000, replace=TRUE)
  )
}
res_y <-
  microbenchmark::microbenchmark(
    digest::digest(y, "md5"     ),
    digest::digest(y, "sha1"    ),
    digest::digest(y, "crc32"   ),
    digest::digest(y, "sha256"  ),
    digest::digest(y, "sha512"  ),
    digest::digest(y, "xxhash32"),
    digest::digest(y, "xxhash64"),
    digest::digest(y, "murmur32"),
    dp_hash(y),
    times = 10
  )

res_y


z <- sample(letters, 10^6, replace = TRUE)
res_z <-
  microbenchmark::microbenchmark(
    digest::digest(z, "md5"     ),
    digest::digest(z, "sha1"    ),
    digest::digest(z, "crc32"   ),
    digest::digest(z, "sha256"  ),
    digest::digest(z, "sha512"  ),
    digest::digest(z, "xxhash32"),
    digest::digest(z, "xxhash64"),
    digest::digest(z, "murmur32"),
    dp_hash(x),
    times = 10
  )

res_z









