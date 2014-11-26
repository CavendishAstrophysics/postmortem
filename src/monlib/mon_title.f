*+MON_TITLE

       subroutine mon_title (ifile, text, isamp1, isamp2, istep,
     :                                            ra, dec, header)
C      -----------------------------------------------------------
C
C  Initialises header text for monitor commands.
C
C  Given:
C      IFILE         integer       sample file logical unit number
C      TEXT          char*(*)      caption text
C      ISAMP1        integer       first sample
C      ISAMP2        integer       last sample
C      ISTEP         integer       sampling rate in samples
C      RA, DEC       real*8        phase centre
C
C  Returned:
C      HEADER        char(4)*80    header text
C
C  Initialises four lines of header text to introduce output from the
C  monitor system commands, for the sample file currently opened on
C  logical unit IFILE.  The title includes the sample range, start and
C  stop times, and the effective sampling interval in seconds.
C
C  [DJT, modified by PA for V1 and V2 CT use 9/11/88]
*-
       character  text*(*), header(4)*80
       integer    ifile, isamp1, isamp2, istep
       real*8     ra, dec
c
       include '/mrao/include/constants.inc'
       include '/mrao/include/chrlib_functions.inc'
c
       real*8     ra_samp, dec_samp
       character  file*64, samp1*5, samp2*5
       integer    integ_time, samp_integ
       integer    itime(3), isecs1, isecs2, isecs
       integer    i1, i2, l1, l2, chr_lenb, length, status

c find file name from unit number
       inquire (unit=ifile, name=file)

c set text representation of file name
       length=chr_lenb(text)
c      i1=index(file,')')+1
c      i2=index(file(i1:),':')+i1-2
       i2=chr_ilstc(file,'/') - 1
       i1=chr_ilstc(file(1:i2),'/') + 1

c find integration time
       call enq_integ( ifile, integ_time, samp_integ, status )
       isecs = integ_time/10

c representation of sample range
       call chr_chitoc(isamp1,samp1,l1)
       call chr_chitoc(isamp2,samp2,l2)
       call chr_chljus(samp1,l1)
       call chr_chljus(samp2,l2)
c
       header(1)=file(i1:i2)
       write(header(2),'(5A)')
     :   text(1:length),',  samples ',samp1(1:l1),' to ',samp2(1:l2)
       write(header(3),'(A,28X,A,10X,A)') 'Phase centre','starting','ST'
       write(header(4),'(A,I5,A,10X,A)')
     :   'Effective sampling interval',isecs,' secs,  stopping','ST'
c
       status=0
       call chr_chdtos(ra/const_h2r,1,header(3)(13:24),length)
       call chr_chdtos(dec/const_d2r,1,header(3)(26:37),length)
       call read_rt(ifile,1,isamp1,ra_samp,dec_samp,isecs1,status)
       call read_rt(ifile,1,isamp2,ra_samp,dec_samp,isecs2,status)
       call util_stohms(isecs1/10,itime)
       call chr_chtime(itime,header(3)(50:57),length)
       call util_stohms(isecs2/10,itime)
       call chr_chtime(itime,header(4)(50:57),length)
c
       end


