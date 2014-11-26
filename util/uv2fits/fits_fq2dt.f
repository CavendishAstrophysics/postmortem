*+FITS_FQ2DT - Frequency table to disc, and thence to tape
*
*
       subroutine fits_fq2dt(status)
*
*  The frequency table is used in conjunction with the 'IF' axis construct
*  to describe the subbands present in Ryle Telescope data.  Entries in
*  the frequency table contain offsets for each subband from the reference
*  frequency specified in the 'FREQ' axis.  See Section 14.4.2 of the
*  'GOING AIPS' manual.
*
*  DJT                16 August 1991 - introduced for Ryle Telescope
*                     16 April 1992 - revised
*
*_

       integer status

       include '/mrao/post/include/global_constants.inc'
       include '/mrao/include/constants.inc'
       include 'uv2fits_common.inc'

       character scratch*30

       integer i, j, id

       real*8 one,zero
       real*8 freq1,freq2
       real*8 cfreq1,cfreq2
       real*8 ch_width,band_width

       integer max_data
       parameter (max_data=1+max_subb*9)
       integer*2 data(max_data)

       real*8 dtemp
       real*4 rtemp
       integer*2 itemp(max_subb*4)
       equivalence (itemp,rtemp,dtemp)

* Entry

       if(status.ne.0) return

       one=1.d0
       zero=0.d0

c      scratch='fqh:out'
c      call io_opefil(ddev,scratch,'write',0,status)
       call io_opescr(ddev,scratch,'write',0,status)
       write(ddev,1000) 8,2,2+18*nsb,1
 1000  format(
     :  'XTENSION= ',10h'A3DTABLE',11x,'/ Extension type'/
     :  'BITPIX  = ',17x,i3,1x,'/ Binary data'/
     :  'NAXIS   = ',17x,i3,1x,'/ Table is a matrix'/
     :  'NAXIS1  = ',17x,i3,1x,'/ Width of table in bytes'/
     :  'NAXIS2  = ',17x,i3,1x,'/ Number of entries in table')

       write(ddev,1001) 0,1,5,1,nsb
 1001  format(
     :  'PCOUNT  = ',17x,i3,1x,'/ Number of random parameters'/
     :  'GCOUNT  = ',17x,i3,1x,'/ Number of data groups'/
     :  'TFIELDS = ',17x,i3,1x,'/ Number of fields in each row'/
     :  'EXTNAME = ',10h'AIPS FQ ',11x,'/ Frequency (IF) table'/
     :  'EXTVER  = ',17x,i3,1x,'/ Version number'/
     :  'NO_IF   = ',17x,i3,1x,'/ Number of IFs')

       write(ddev,1002) nsb,'/'
 1002  format(
     :  'TFORM1  = ',10h'1I      ',11x,'/'/
     :  'TTYPE1  = ',10h'FRQSEL  ',11x,'/'/
     :  'TFORM2  = ''',i1,'D      ''',11x,'/'/
     :  'TTYPE2  = ',10h'IF FREQ ',11x,'/'/
     :  'TUNIT2  = ',10h'HZ      ',11x,a)

       write(ddev,1003) nsb,nsb,nsb,'/'
 1003  format(
     :  'TFORM3  = ''',i1,'E      ''',11x,'/'/
     :  'TTYPE3  = ',10h'CH WIDTH',11x,'/'/
     :  'TUNIT3  = ',10h'HZ      ',11x,'/'/
     :  'TFORM4  = ''',i1,'E      ''',11x,'/'/
     :  'TTYPE4  = ',17h'TOTAL BANDWIDTH',4x,'/'/
     :  'TUNIT4  = ',10h'HZ      ',11x,'/'/
     :  'TFORM5  = ''',i1,'I      ''',11x,'/'/
     :  'TTYPE5  = ',10h'SIDEBAND',11x,'/'/
     :  'TUNIT5  = ',10h'HZ      ',11x,a)

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

* Write frequency data to tape

       data(1) = 1

       id = 1
       call enq_iba_freq(lun,isb(1),freq1,status)
       do i=1,nsb
         call enq_iba_freq(lun,isb(i),freq2,status)
         dtemp = freq2-freq1
         call util_zrlr64(dtemp,dtemp,1)
         do j = 1,4
           data(id+j) = itemp(j)
         enddo
         id = id+4
       enddo

       call enq_ich_freq(lun,ich(1),cfreq1,status)
       call enq_ich_freq(lun,ich(nch),cfreq2,status)
       ch_width = (cfreq2-cfreq1)/(nch-1)
       band_width = cfreq2-cfreq1

       rtemp = ch_width
       call util_zrlr32(rtemp,rtemp,1)
       do i = 1, nsb
         do j = 1,2
           data(id+j) = itemp(j)
         enddo
         id = id+2
       enddo

       rtemp = band_width
       call util_zrlr32(rtemp,rtemp,1)
       do i = 1, nsb
         do j = 1,2
           data(id+j) = itemp(j)
         enddo
         id = id+2
       enddo

       do i = 1, nsb
         data(id+i) = 1
       enddo
       id = id + nsb

       call fits_d2t('write',data,id,status)

       data(1)=0
       call fits_d2t('fill',data,0,status)

       close(ddev)

       end
