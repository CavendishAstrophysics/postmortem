*+FITS_AN2DT - Antenna table to disc, and thence to tape
*
*
       subroutine fits_an2dt(status)
*
*
*  S.Kenderdine       27 July 1988
*                     28 July 1988 - control tables include files
*  DJT                August 1991  - revised for Ryle Telescope
*
*_

       integer status

       include 'uv2fits_common.inc'
       include '/mrao/include/constants.inc'

       character scratch*30

       integer max_ae, max_sp, max_sb, max_ch
       integer iae

       real*8 conv
c      real*8 uscale
       real*8 xyz(3)

* Entry

       if(status.ne.0) return

c      scratch='anh:out'
c      call io_opefil(ddev,scratch,'write',0,status)
       call io_opescr(ddev,scratch,'write',0,status)
       call enq_teldef(lun, max_ae, max_sp, max_sb, max_ch, status)
       write(ddev,1000) 8,2,80,max_ae
 1000  format(
     :  'XTENSION= ',10h'TABLE   ',11x,'/ Extension type'/
     :  'BITPIX  = ',17x,i3,1x,'/ 8 = ASCII'/
     :  'NAXIS   = ',17x,i3,1x,'/ 2 axes'/
     :  'NAXIS1  = ',17x,i3,1x,'/ 80 characters wide'/
     :  'NAXIS2  = ',17x,i3,1x,'/ Number of entries in table')

       write(ddev,1001) 0,1,5,1
 1001  format(
     :  'PCOUNT  = ',17x,i3,1x,'/ No random parameters'/
     :  'GCOUNT  = ',17x,i3,1x,'/ 1 block of data'/
     :  'TFIELDS = ',17x,i3,1x,'/ 5 fields per row of table'/
     :  'EXTNAME = ',10h'AIPS AN ',11x,'/ Antenna table'/
     :  'EXTVER  = ',17x,i3,1x,'/ Version number')

       write(ddev,1002) 1,9,'/'
 1002  format(
     :  'TBCOL1  = ',17x,i3,1x,'/'/
     :  'TFORM1  = ',10h'I4      ',11x,'/'/
     :  'TTYPE1  = ',10h'ANT NO. ',11x,'/'/
     :  'TBCOL2  = ',17x,i3,1x,'/'/
     :  'TFORM2  = ',10h'A4      ',11x,'/'/
     :  'TTYPE2  = ',10h'STATION ',11x,a)

c      uscale=1.0/4.0d11
c      write(ddev,1003) 17,uscale,36,uscale,55,uscale
       write(ddev,1003) 17,36,55,'/'
 1003  format(
     :  'TBCOL3  = ',17x,i3,1x,'/'/
     :  'TFORM3  = ',10h'E15.6   ',11x,'/'/
     :  'TTYPE3  = ',10h'LX      ',11x,'/'/
     :  'TUNIT3  = ',10h'METERS  ',11x,'/'/
     :  'TBCOL4  = ',17x,i3,1x,'/'/
     :  'TFORM4  = ',10h'E15.6   ',11x,'/'/
     :  'TTYPE4  = ',10h'LY      ',11x,'/'/
     :  'TUNIT4  = ',10h'METERS  ',11x,'/'/
     :  'TBCOL5  = ',17x,i3,1x,'/'/
     :  'TFORM5  = ',10h'E15.6   ',11x,'/'/
     :  'TTYPE5  = ',10h'LZ      ',11x,'/'/
     :  'TUNIT5  = ',10h'METERS  ',11x,a)

       write(ddev,1004)
 1004  format('END')

* End of generating extension header; transcribe to tape

       endfile(ddev)
       rewind(ddev)

  200  read(ddev,1005,end=201) line
 1005  format(a)
       call fits_d2t('write',i2line,40,status)
       goto 200

  201  line=' '
       call fits_d2t('fill',i2line,0,status)

       close(ddev)

* Open antenna data scratch file, and eventually transfer to tape

c      scratch='and:out'
c      call io_opefil(ddev,scratch,'write',0,status)
       call io_opescr(ddev,scratch,'write',0,status)

       conv = const_c
       do iae=1,max_ae
         call enq_geometry(lun, iae, xyz, status )
         write(ddev,1010) iae,xyz(2)*conv,-xyz(1)*conv,xyz(3)*conv
c        write(ddev,1010) iae,xyz(1)*conv,xyz(2)*conv,xyz(3)*conv
 1010    format(i4,8x,3(4x,e15.6))
       enddo

       endfile(ddev)
       rewind(ddev)

  300  read(ddev,1011,end=301) line
 1011  format(a)
       call fits_d2t('write',i2line,40,status)
       goto 300

  301  line=' '
       call fits_d2t('fill',i2line,0,status)

       close(ddev)

       end
