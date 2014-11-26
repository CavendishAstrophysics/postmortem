C Work-Space Array for POSTMORTEM
C -------------------------------

       integer       work_size
       parameter    (work_size = 300000)
       integer       post_work(work_size)
       common /post/ post_work
