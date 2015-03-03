
LibHaru/SML
===============================================================

.. contents::


StandardML(SML) [#smlnj]_ binding library to libHaru. [#libharu]_



Requirements
---------------------------------------------------------------

This binding library is developped with...

- libHaru 2.4.0
- SML/NJ 110.77


Environment
---------------------------------------------------------------

Linux/x86 is only supported platform.
But, code depends on Linux and/or x86 is not contained.


Build
---------------------------------------------------------------

If SML/NJ and LibHaru is installed on your system,
just perform **make** is required for building libharu/sml.

::
    $ make


If the LibHaru has been installed in locale different from default path (/usr/{include,lib}...),
Specify the path to the header and library files of LibHaru explicitly.

::
    $ HPDF_INCLUDE_DIR=/path/to/include \
      HPDF_SHARED_LIB=/path/to/libhpdf.so \
      make



Sample
---------------------------------------------------------------

Some demo programs are ported from official demo programs.
Execute these demos, specify **runsample** target as a make target like below:

::
    $ make runsample



---------------------------------------------------------------

.. [#smlnj] http://www.smlnj.org/
.. [#libharu] http://libharu.org/


