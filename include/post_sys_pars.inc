C PostMortem System Parameters
C ----------------------------
* /mrao/post/include/post_sys_pars.inc

C P. Alexander   04/02/90
C DJT            09/07/92  Unix version
C GGP            01/06/99  separate init file for test version
C GGP            06/06/2001 revised gt... files

C This file contains definitions of system parameters.
C
C system version and date
       character    post_version*6
       parameter   (post_version = '5')
       character    post_date*15
       parameter   (post_date = 'June 2001')

* initialisation files

       character    initialisation_file*13
       parameter   (initialisation_file='post_init.com')
       character    system_init*24
       parameter   (system_init='/mrao/post/post_init.com')
       character    system_test*25
       parameter   (system_test='/mrao/post/tpost_init.com')

C help file
       character    help_file*30
       parameter   (help_file = '/mrao/help/post/post_post.help')
C
C news file
       character    news_file*30
       parameter   (news_file = '/mrao/help/post/news_file.news')
C
C banner (displayed on initialisation)
       character    banner_file*24
       parameter   (banner_file = '/mrao/post/post_banner')
C
C error log
       character    error_file*26
       parameter   (error_file = '/mrao/post/post_error.elog')
C
C
C files for gain tables for RT
       character*29   RT_gt_index
       parameter     (RT_gt_index = '/mrao/post/data/rt/index.data')
       character*29   RT_gains
       parameter     (RT_gains    = '/mrao/post/data/rt/gains.data')
       character*31   RT_v_gains
       parameter     (RT_v_gains  = '/mrao/post/data/rt/v_gains.data')
C
C error file for flag table accesses
       character   flag_error_file*34
       parameter  (flag_error_file =
     :                            '/mrao/post/include/flag_errors.inc')
C
C default source list
       character   def_source_list*40
       parameter  (def_source_list = '/mrao/post/data/source_list')
C
C miscellaneous defaults
       character   default_lsf_extract*19
       parameter  (default_lsf_extract = 'SAMPLE-FILE-MAP.MAP')
C
C
