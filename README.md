OpenWGA CMS Server
================================

Building
--------

Run

    cd OpenWGA
    ./gradlew clean build

Distributions are:

- OpenWGA/server/build/libs/openwga-[version]-ce.war - The JavaEE web application archive
- OpenWGA/distros/wgaDeb/distributions/openwga....deb - The debian package


Developing in Eclipse
---------------------

Make sure you have Eclipse Mars and at least Buildship 1.0.14 installed.


Versioning
----------

Specify OpenWGA version in file "gradle.properties" in main project folder.
