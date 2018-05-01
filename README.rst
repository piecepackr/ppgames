Trevor's piecepack rules source code
====================================

NB. this repo just contains miscellaneous source code.  Compiled pdf's are hosted at the `Piecepack Wiki <http://www.ludism.org/ppwiki/TrevorLDavis>`_.

Dependencies
------------

1) `Piecepack Grahpics R Package <http://www.ludism.org/ppwiki/PiecepackRPackage>`_ installed (and all dependencies installed) plus the project git cloned and all the demo images built (i.e. have ran ``rake all``).  There should be a symbolic link ``configurations`` linking with the ``configurations`` subdirectory in that (cloned) project directory and a symbolic link called ``images/pdf`` linking to the ``pdf/components`` subdirectory in that (cloned) project directory.
2) ``rake``, ``rst2pdf``, ``pandoc``, ``texlive-xetex``, etc. in order to compile the reStructuredText into pretty pdfs.

License
-------

Unless otherwise specified everything in this repo is licensed under the `CC BY-SA 4.0 license <https://creativecommons.org/licenses/by-sa/4.0/>`_.
