#!/bin/sh

npm --version > /dev/null 2>&1

if [ $? != 0 ]; then
  echo 'Please install the Node package manager (npm).'
  exit 1
fi

if [ $(npm list --parseable mathoid | grep -c mathoid) == 0 ]; then
  npm install mathoid
fi

#if [ ! -d mathoid ]; then
#  git clone https://github.com/wikimedia/mathoid
#  cd mathoid
#  git tag 0.6.3 fe51562923a26cfefc34bbde1045976aef1ff632
#  git co 0.6.3
#  cd ..
#fi

# Make sure you can run the following command:
#
#  $ npm run mathoid
#
# or
#
# ./node_modules/mathoid/server.js -c mathoid-config.yaml
#
# Then curl the following URL
#
#  $ curl -d 'q=x^2&type=asciimath' localhost:10044/svg > test.svg
#
# You should see x to the power of 2 rendered when you visit test.svg in the browser.
#
# Press Ctrl+C when you are done testing.
#
# If you want to use the STIX-Web fonts to render the math,
# add the following property to the MathJax configuration in node_modules/mathoid/app.js:
#
#   SVG: {
#     font: 'STIX-Web'
#   }
#
