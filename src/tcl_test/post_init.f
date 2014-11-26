       subroutine post_init( s )
C      -------------------------
C
C Updated:
C   error status
       integer    s
C
C     Cambridge Synthesis Telescopes Observation Reduction Package
C
C     Version X0.1
C
C*
C ======================================================================
C
C Local constant and variable declarations
C
          include '/mrao/post/include/post_common.inc'
          include '/mrao/post/include/post_sys_pars.inc'
          include '/mrao/post/include/post_work_array.inc'

C         Sample file names.
              character*(80)  def_sf
C         LSF numbers and keys.
              integer         def_lsf
              common /tclpost/ def_sf, def_lsf

C initialise for use of IOLIB and command language
      s = 0
      call io_initio
      call io_setlog( error_file, s )

C define defaults
      def_sf  = ' '
      def_lsf = -1
      call getenv( 'SAMPDIR', def_dir )

C define default plot device
      plot_device = '/xwindow'

      end
