-*- text -*-

***** About Cells *****

(Installation instructions follow)

Cells is a mature, stable extension to CLOS that allows you to create
classes, the instances of which have slots whose values are determined
by a formula. Think of the slots as cells in a spreadsheet (get it?),
and you've got the right idea. You can use any arbitrary Common Lisp
expression to specify the value of a cell. The Cells system takes care
of tracking dependencies among cells, and propagating values. It is
distributed under an MIT-style license.

Documentation/support is in the form of:

   the cells-devel mailing list (users and developers both welcome)
   .\docs\01-cell-basics.lisp
   .\docs\motor-control.lisp ;; actually Bill Clementson's blog entry
   extensive examples in the Cells-test regression test suite 
   the companion Celtk module, which happens also to provide a substantial and 
        growing portable, native Common Lisp GUI. 

The above examples have all been tested against the current release of Cells.
Now in .\doc is cells-overview.pdf. That is pretty rough and obsolete in re the
code, but some of it might be enlightening.

Cells is written in portable ANSI Common Lisp.  It makes very
light use of the introspective portions of the MOP, and contains a few
workarounds for shortcomings in common implementations. 

Cells is known to currently work on the following Lisp implementations:

  * Allegro
  * SBCL
  * CLISP
  * LispWorks
  * OpenMCL

Partially supported are:

  * CMUCL
  * Corman Lisp
  * MCL

One of the Cells tests fails with CMUCL.  This appears to be caused by
a bug in CMUCL's  CLOS implementation, but has not been investigated in
great depth.

Cells is believed to work with Corman CL, but has not been recently
tested.  In the past, MCL was supported, but a it does not currently
pass the test suite.  Ressurecting full support for any of these
implementations should be easy.

Porting Cells to an unsupported but ANSI-conforming Lisp
implementation should be trivial: mostly a matter of determining the
package where the MOP lives.  In reality, however, you might have to
find workarounds for bugs in ANSI compliance.

***** Installation *****

[ Cells follows the usual convention for asdf and asdf-installable
  packages.  If you know what that means, that's all you need to
  know. ]

Installation is trivial for asdf-install users:

  (asdf-install:install :cells)

Users without asdf-install will need to download the distribution from
common-lisp.net.  If your implementation does not come with ASDF,
please complain to the implementor, then load the asdf.lisp file
included in the Cells distribution.

Unpack the distribution where you will.

Unix users: If you do not already have an asdf central registry,
create a directory calld asdf-registry under your home directory and
push this onto asdf:*central-registry*.  Create symlinks there to the
cells.asd and cells-test.asd files in the distribution.  Alternately,
follow the instructions for Windows users.

Windows and Classic Mac users: Push the directory where you unpacked
the Cells distribution onto asdf:*central-registry*.

You can now load Cells in the usual manner for asdf.

SLIME:

  ,load-system cells

SBCL:

  (require :cells)

Other systems:

  (asdf:oos 'asdf:load-op :cells)

You may wish to run the test suite.  To do so, use asdf to load the
:cells-test system.
