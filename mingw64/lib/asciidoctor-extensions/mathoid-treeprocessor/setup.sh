#!/bin/sh

npm --version > /dev/null 2>&1

if [ $? != 0 ]; then
  echo 'Please install the Node package manager (npm).'
  exit 1
fi

if [ $(npm list phantomjs | grep -c phantomjs) == 0 ]; then
  npm install phantomjs
fi

if [ ! -d mathoid ]; then
  #git clone --recursive https://github.com/gwicke/mathoid
  #git clone -b integration --recursive https://github.com/mojavelinux/mathoid
  git clone --branch debian/0.2.4 --recursive https://git.wikimedia.org/git/mediawiki/services/mathoid.git
fi

# Make sure you can run the following command:
#
# cd mathoid
# phantomjs main.js
#
# Then visit the following URL in your browser:
#
# http://localhost:16000/?q=x^2&type=asciimath
#
# You should see x to the power of 2 rendered.
# Press Ctrl+C when you are done.
#
# If you want to use the STIX-Web fonts to render the math,
# add the following property to the MathJax configuration in mathoid/index.html:
#
#   SVG: {
#     font: 'STIX-Web'
#   }
#
