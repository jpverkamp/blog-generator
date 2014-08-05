BUCKET='s3://blog-s3.jverkamp.com/'
SITE_DIR='_build'

echo '--> Rebuilding site'
racket blog.rkt --bypass-cache > /dev/null

cd $SITE_DIR

echo '--> Converting line endings'
find . -type f -exec dos2unix {} \; 2>1 > /dev/null

echo '--> Syncing to s3'
s3cmd sync --delete-removed . $BUCKET

cd ..