C     *****************************************************************
C
C     Logical Sample File Definition Common
C     -------------------------------------
C
C     This common block contains the current LSF definition record. It
C     is defined using the LSF record stored in the file:
C
C         /mrao/post/include/lsf_record.inc
C
C     The LSF definition can only refer to one particular physical
C     sample file (PSF). If the the logical sample file is saved then
C     it is saved in the file:
C             (pack-two:<telescope name>)<PSF file name>:lsf
C
C     Because of this convention there is no explicit pointer between
C     the PSF and the LSF, this being done by the PSF file name. Each
C     LSF takes a page in this save file and is uniquely identified
C     by the lsf_key, which is the SINTRAN file date of when the LSF
C     was saved. If the LSF has not been saved, then this key is zero,
C     and the lsf_name is also blank.
C
C
C     *****************************************************************

      include '/mrao/post/include/lsf_record.inc'

C     Define the common block.

      common      / lsfcom /  log_samp_file
