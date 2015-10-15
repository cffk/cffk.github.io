                    Scripts for manipulating molecules

   (c) 2004, 2005, Sarnoff Corporation, Princeton, NJ, USA
   $Id: README.txt 6001 2005-06-23 15:51:30Z ckarney $
     _________________________________________________________________

   This  collection  of  scripts  was  written by Sarnoff to help prepare
   organic  molecules  for  simulation. These scripts rely heavily on the
   following software packages:
    1. Antechamber, http://amber.scripps.edu/antechamber/antechamber.html
    2. RESP, http://amber.scripps.edu/ftp/plep.tar.gz
    3. OpenBabel, http://openbabel.sourceforge.net
    4. Gamess, http://www.msg.ameslab.gov/GAMESS/GAMESS.html
    5. Gromacs, http://www.gromacs.org

   Our thanks to the authors of these packages.

     These  scripts  are  released under the GNU General Public License.
     See  LICENSE.txt for details. In particular note that this software
     comes with NO WARRANTY.

   This  software was created  under Sarnoff's  Bug-to-Drug(R) Engineered
   Identification and Countermeasures Program which is funded by the U.S.
   Army   Medical   Research   and   Materiel  Command   under   Contract
   No. DAMD17-03-C-0082.

   The people involved are:
     * Charles Karney <ckarney@sarnoff.com>
     * Vincent Finley
     * Jason Ferrara <jeferrara@sarnoff.com>
     * Qiang Wang

   The complete set of scripts is available at b2dscripts-r1.tar.gz

   Questions,  comments,  and improvements on this work should be sent to
   ckarney@sarnoff.com

   Here are the main objectives of these scripts:
     * Allow  Gamess  to  be used in conjunction with Antechamber for the
       assignment of partial charges by the RESP or AM1-BCC methods.
     * Add  hydrogens  to  a PDB file using Gromacs and adjust atom names
       and certain bond angles to conform to Amber's rules.

   A couple of shortcomings of Antechamber are addressed.
     * The charges produced by the AM1-BCC method are symmetrized between
       chemically equivalent atoms (using respgen's symmetry detection).
     * Antechamber produces several temporary files which mean that it is
       unsafe  to  run multiple instances within the same directory. This
       is addressed by placing the temporary files in a unique directory.

   These  scripts  are  written  as  filters expecting input on stdin and
   producing  output  on  stdout.  Diagnostic  messages appear on stderr.
   Generally  a  -h  option  produces  a  description of the script. A -d
   option  causes  the  temporary directory to be retained for diagnostic
   purposes.  The  auxiliary  programs  that these scripts call should be
   located  in  your  PATH.  These  scripts were written and tested under
   Linux.  They should work on other Unix platforms. (It is possible that
   we rely on GNU awk extensions. Let us know if this is the case.)

   Here   is   a   quick   description  of  the  scripts.  More  complete
   documentation is obtained by running the script with the -h option.
     * gjftogcrt:  Converts  a  general  Gaussian input file (possibly in
       mixed   Z-matrix   and  Cartesian  form)  into  a  pure  Cartesian
       representation.
     * gjftogms:  Converts  a  Gaussian input file to a Gamess format. It
       does  this  by invoking gjftogcrt and using Antechamber to convert
       the  result  to a Z-matrix form. The Z-matrix input allows the use
       of delocalized coordinates during geometry optimization by Gamess.
       acreorder  is  used  to  reorder  of  the  atoms in order that the
       Z-matrix makes sense.
     * gmstoresp:  Converts  the  log  file  produced  with  Gamess to an
       Antechamber  file with RESP charges. The input file for Gamess can
       be  created  with  gjftogms  -r.  OpenBabel is used to extract the
       coordinate  information  for the output file. RESP and Antechamber
       are used to calculate the charges and create the output file. (The
       starting  point for this script was one written by Hans De Winter,
       Katholieke  Universiteit Leuven, hans@s151h14.rega.kuleuven.ac.be,
       which is provided with the RESP distribution.)
     * gmstobcc:  Converts  the  log  file  produced  with  Gamess  to an
       Antechamber  file  with AM1-BCC charges. The input file for Gamess
       can  be created with gjftogms -a. OpenBabel is used to extract the
       coordinate  information  for  the output file. A work-alike divcon
       output  file  is  created  and  Antechamber  is invoked requesting
       AM1-BCC  charges.  (This  invokes  divcon for which we use a dummy
       script.) Finally we invoke chargesym to symmetrize the charges.
     * divcon: A dummy divcon program (which merely copies the previously
       prepared  output  file to divcon.out). The Makefile copies this to
       $AMBERHOME/exe.
     * chargesym:  This  symmetrizes  the  charges in an Antechamber file
       using respgen to identify equivalent atoms.
     * acreorder: Reorders the atoms in an Antechamber file following the
       bonds  from  a  starting  atom.  This  is sometimes necessary as a
       precursor to converting the molecule to a Z-matrix representation.
     * addhydrogens:  Adds  the  hydrogens  to the protein in a PDB file.
       This  invokes  the  Gromacs  tool,  pdb2gmx, to add hydrogens. The
       result  is  fixed up to comply with Amber conventions by g96topdb.
       gromacs.patch  contains  a  patch  to gromacs 3.2.1 to correct the
       angles in NH2 groups.
     * g96topdb:  Reads in a Gromacs96 (g96) file, performs OPLS to Amber
       atom  type and residue translation, replaces hydrogen bond lengths
       with  bond lengths consistent with Amber force field, corrects for
       the  SH  bond angle in CYS, and finally writes the result to a pdb
       file.  Amber  residue variants by including the variant code (ASH,
       CYX, GLH, HID, HIE, or LYN) in columns 73-75 of the output.
     * gjfrenumber:  Reads  in  a  Gaussian  input file and renumbers the
       atoms  and  converts  numerical  indexes  in  Z-matrix  lines into
       symbolic  indexes.  This  would  allow  two  molecules in Z-matrix
       format  to  be  bonded together with a straightforward edit of the
       files.
     * antechamberfilt  and  babelfilt:  These  are  simple  scripts that
       change antechamber and babel into filters so that they can be used
       in pipes.
     _________________________________________________________________

   Charles Karney <ckarney@sarnoff.com>
   Sarnoff Corporation, 201 Washington Rd   
   Princeton, NJ 08543-5300
