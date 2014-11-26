

*+PRINT_GEOM_RYLE

       subroutine print_geom_ryle (ifile, status)
C      ------------------------------------------
C
C  Executes the PRINT-GEOMETRY command.
C
C  Given:
C      IFILE         integer       sample file logical unit
C      STATUS        integer       status value
C
C  Support routine for PRINT_GEOMETRY for the Ryle telescope.
C
C  PA, 26/2/89
C  DJT, 12/1/90
*-
       integer       ifile, status
c
       include '/mrao/include/iolib_functions.inc'
c
       integer       iout
       integer       i, iae, iba, ich, nae, nba, nch, nsp
       integer       aerial, subband, channel
       real*8        xyz(3)
       character*1   band(5)
       data          band / 'A', 'B', 'C', 'D', 'E'/

       if (status.ne.0) return

       call io_enqout(iout)

       call enq_obsdef( ifile, nae, nsp, nba, nch, status)
       write(iout,'(16X,3(9X,A))') 'X','Y','Z'
       do iae = 1,nae
         write(iout,*)
         call enq_iae_code( ifile, iae, aerial, status)
         do iba = 1,nba
           call enq_iba_code( ifile, iba, subband, status)
           do ich = 1,nch
             call enq_ich_code( ifile, ich, channel, status)
             call enq_chgeom( ifile, aerial, subband, channel,
     :                                                      xyz, status)
             write (iout,1) aerial, band(subband),channel,
     :                                                  (xyz(i), i=1,3)
  1          format(1X,'Ae',I2,' sb ',A1,' ch',I2,' :',2X,3F10.2)
             if (io_attn(status)) goto 2
           end do
         end do
       end do

  2    continue

       end
