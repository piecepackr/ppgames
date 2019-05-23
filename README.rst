ppgames: Piecepack game diagrams and rules
==========================================
.. image:: https://travis-ci.org/trevorld/ppgames.png?branch=master
    :target: https://travis-ci.org/trevorld/ppgames
    :alt: Build Status

.. image:: https://ci.appveyor.com/api/projects/status/github/trevorld/ppgames?branch=master&svg=true 
    :target: https://ci.appveyor.com/project/trevorld/ppgames
    :alt: AppVeyor Build Status

.. image:: https://img.shields.io/codecov/c/github/trevorld/ppgames/master.svg
    :target: https://codecov.io/github/trevorld/ppgames?branch=master
    :alt: Coverage Status

.. image:: http://www.repostatus.org/badges/latest/wip.svg
   :alt: Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.
   :target: http://www.repostatus.org/#wip

``ppgames`` is an R_ package designed to make piecepack_ game diagrams, rulesets, and rulebooks.

.. _piecepack: http://www.ludism.org/ppwiki/HomePage
.. _piecepackr: https://github.com/trevorld/piecepackr
.. _R: https://www.r-project.org/

Installation
------------

To install the ``ppgames`` R package use the following commands in R_:

.. code:: r

   install.packages("remotes")
   remotes::install_github("trevorld/piecepackr")
   remotes::install_github("trevorld/ppgames")

If you want to make rulebooks you'll need ``xelatex`` and if you want to make rulesets you'll need a recent version of ``pandoc``.  The following instructions should work on Ubuntu:

.. code:: bash

    sudo apt install texlive-xetex
    sudo apt install cabal-install
    cabal update # add $HOME/.cabal/bin to $PATH
    cabal install pandoc

License
-------

Unless otherwise specified everything in this repo is licensed under the `CC BY-SA 4.0 license <https://creativecommons.org/licenses/by-sa/4.0/>`_.
