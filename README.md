# PDFtalk for Gemstone
Library for reading and writing PDF files for [`GsDevKit for Gemstone`](https://github.com/GsDevKit/GsDevKit_home).

See the wiki page [PDFtalk for Gemstone](https://wiki.pdftalk.de/doku.php?id=pdftalk4gemstone) for details.

Prerequisites: 
* install [`GsDevKit for Gemstone`](https://github.com/GsDevKit/GsDevKit_home)
* Log in as `DataCurator`

Following files are available:
* **PDFtalk.gs** is the runtime library in Gemstone topaz source format
* **PDFtalkWithCMaps.gs** is the same with all standard CMaps included (needed to work with Asian texts)
* **PDFtalkTesting.gs** contains tests and demos. Needs `PDFtalk.gs` or `PDFtalkWithCMaps.gs`
* **Report4PDF.gs** is the Report Writer of Bob Nemec based on the PDFtalk library. Needs `PDFtalk.gs` or `PDFtalkWithCMaps.gs`
* **Report4PDFTest.gs** contains tests (which all fail unfortunately). Needs `Report4PDF.gs`
