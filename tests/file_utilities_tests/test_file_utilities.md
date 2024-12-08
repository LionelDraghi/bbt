# File_Utilities.Short_Path unit tests

1. Subdir with default Prefix : OK
Short_Path (From_Dir => "/home/tests",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = mysite/site/d1/idx.txt

2. Dir with final / : OK
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = mysite/site/d1/idx.txt

3. subdir with Prefix : OK
Short_Path (From_Dir => "/home/tests",
            To_File  => "/home/tests/mysite/site/d1/idx.txt",
            Prefix   => "./") = ./mysite/site/d1/idx.txt

4. Sibling subdir : OK
Short_Path (From_Dir => "/home/tests/12/34",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = ../../mysite/site/d1/idx.txt

5. Parent dir : OK
Short_Path (From_Dir => "/home/tests/12/34",
            To_File  => "/home/tests/idx.txt") = ../../idx.txt

6. Other Prefix : OK
Short_Path (From_Dir => "/home/tests/12/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt",
            Prefix   => "$PWD/") = $PWD/../mysite/site/d1/idx.txt

7. Root dir : OK
Short_Path (From_Dir => "/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = /home/tests/mysite/site/d1/idx.txt

8. File is over dir : OK
Short_Path (From_Dir => "/home/tests/mysite/site/d1",
            To_File  => "/home/readme.txt") = ../../../../readme.txt

9. File is over Dir, Dir with final / : OK
Short_Path (From_Dir => "/home/tests/mysite/site/d1/",
            To_File  => "/home/readme.txt") = ../../../../readme.txt

10. File is the current dir : OK
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests") = ./

11. File is over Dir, Dir and File with final / : OK
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests/") = ./

12. No common part : OK
Short_Path (From_Dir => "/home/toto/src/tests/",
            To_File  => "/opt/GNAT/2018/lib64/libgcc_s.so") = /opt/GNAT/2018/lib64/libgcc_s.so

# File_Utilities.Short_Path unit tests

# File_Utilities.Short_Path unit tests

# File_Utilities.Short_Path unit tests

1. Subdir with default Prefix : OK
Short_Path (From_Dir => "/home/tests",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = mysite/site/d1/idx.txt

2. Dir with final / : OK
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = mysite/site/d1/idx.txt

3. subdir with Prefix : OK
Short_Path (From_Dir => "/home/tests",
            To_File  => "/home/tests/mysite/site/d1/idx.txt",
            Prefix   => "./") = ./mysite/site/d1/idx.txt

4. Sibling subdir : OK
Short_Path (From_Dir => "/home/tests/12/34",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = ../../mysite/site/d1/idx.txt

5. Parent dir : OK
Short_Path (From_Dir => "/home/tests/12/34",
            To_File  => "/home/tests/idx.txt") = ../../idx.txt

6. Other Prefix : OK
Short_Path (From_Dir => "/home/tests/12/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt",
            Prefix   => "$PWD/") = $PWD/../mysite/site/d1/idx.txt

7. Root dir : OK
Short_Path (From_Dir => "/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = /home/tests/mysite/site/d1/idx.txt

8. File is over dir : OK
Short_Path (From_Dir => "/home/tests/mysite/site/d1",
            To_File  => "/home/readme.txt") = ../../../../readme.txt

9. File is over Dir, Dir with final / : OK
Short_Path (From_Dir => "/home/tests/mysite/site/d1/",
            To_File  => "/home/readme.txt") = ../../../../readme.txt

10. File is the current dir : OK
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests") = ./

11. File is over Dir, Dir and File with final / : OK
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests/") = ./

12. No common part : OK
Short_Path (From_Dir => "/home/toto/src/tests/",
            To_File  => "/opt/GNAT/2018/lib64/libgcc_s.so") = /opt/GNAT/2018/lib64/libgcc_s.so

# File_Utilities.Short_Path unit tests

1. Subdir with default Prefix : OK
Short_Path (From_Dir => "/home/tests",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = mysite/site/d1/idx.txt


File_Utilities.Short_Path tests OK
