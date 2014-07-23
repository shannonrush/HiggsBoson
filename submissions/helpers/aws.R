PutObject <- function(bucket="cloudier", key, body) {
    cmd <- paste("aws s3api put-object --bucket", bucket, "--key", key, "--body", body)
    system(cmd)
}

DeleteObject <- function(bucket="cloudier", key) {
    cmd <- paste("aws s3api delete-object --bucket", bucket, "--key", key)
    system(cmd)
}

LoadObject <- function(bucket="cloudier", key) {
    dir.create("RData")
    url <- paste("https://s3.amazonaws.com/cloudier/", key)
    destfile <- paste("RData/", key)
    download.file(url, destfile=destfile, method="curl")
    load(destfile)
}