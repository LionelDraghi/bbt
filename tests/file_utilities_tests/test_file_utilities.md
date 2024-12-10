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
# File_Utilities.Short_Path unit tests

1. Subdir with default Prefix : OK
Short_Path (From_Dir => "c:\home\tests",
            To_File  => "c:\home\tests\mysite\site\d1\idx.txt") = mysite\site\d1\idx.txt

2. Dir with final / : OK
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = mysite/site/d1/idx.txt

3. subdir with Prefix : NOK ****
Short_Path (From_Dir => "/home/tests",
            To_File  => "/home/tests/mysite/site/d1/idx.txt",
            Prefix   => ".\") = .\mysite/site/d1/idx.txt
Expected ./mysite/site/d1/idx.txt

4. Sibling subdir : NOK ****
Short_Path (From_Dir => "/home/tests/12/34",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = ..\..\mysite/site/d1/idx.txt
Expected ../../mysite/site/d1/idx.txt

5. Parent dir : NOK ****
Short_Path (From_Dir => "/home/tests/12/34",
            To_File  => "/home/tests/idx.txt") = ..\..\idx.txt
Expected ../../idx.txt

6. Other Prefix : NOK ****
Short_Path (From_Dir => "/home/tests/12/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt",
            Prefix   => "$PWD/") = $PWD/..\mysite/site/d1/idx.txt
Expected $PWD/../mysite/site/d1/idx.txt

# File_Utilities.Short_Path unit tests

1. Subdir with default Prefix : OK
Short_Path (From_Dir => "c:\home\tests",
            To_File  => "c:\home\tests\mysite\site\d1\idx.txt") = mysite\site\d1\idx.txt

2. Dir with final / : OK
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = mysite/site/d1/idx.txt

3. subdir with Prefix : NOK ****
Short_Path (From_Dir => "/home/tests",
            To_File  => "/home/tests/mysite/site/d1/idx.txt",
            Prefix   => ".\") = .\mysite/site/d1/idx.txt
Expected ./mysite/site/d1/idx.txt

4. Sibling subdir : NOK ****
Short_Path (From_Dir => "/home/tests/12/34",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = ..\..\mysite/site/d1/idx.txt
Expected ../../mysite/site/d1/idx.txt

5. Parent dir : NOK ****
Short_Path (From_Dir => "/home/tests/12/34",
            To_File  => "/home/tests/idx.txt") = ..\..\idx.txt
Expected ../../idx.txt

6. Other Prefix : NOK ****
Short_Path (From_Dir => "/home/tests/12/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt",
            Prefix   => "$PWD/") = $PWD/..\mysite/site/d1/idx.txt
Expected $PWD/../mysite/site/d1/idx.txt

# File_Utilities.Short_Path unit tests

1. Subdir with default Prefix : OK
Short_Path (From_Dir => "c:\home\tests",
            To_File  => "c:\home\tests\mysite\site\d1\idx.txt") = mysite\site\d1\idx.txt

2. Dir with final / : OK
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = mysite/site/d1/idx.txt

3. subdir with Prefix : OK
Short_Path (From_Dir => "\home\tests",
            To_File  => "\home\tests\mysite\site\d1\idx.txt",
            Prefix   => ".\") = .\mysite\site\d1\idx.txt

4. Sibling subdir : NOK ****
Short_Path (From_Dir => "/home/tests/12/34",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = ..\..\mysite/site/d1/idx.txt
Expected ../../mysite/site/d1/idx.txt

5. Parent dir : NOK ****
Short_Path (From_Dir => "/home/tests/12/34",
            To_File  => "/home/tests/idx.txt") = ..\..\idx.txt
Expected ../../idx.txt

6. Other Prefix : NOK ****
Short_Path (From_Dir => "/home/tests/12/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt",
            Prefix   => "$PWD/") = $PWD/..\mysite/site/d1/idx.txt
Expected $PWD/../mysite/site/d1/idx.txt

# File_Utilities.Short_Path unit tests

1. Subdir with default Prefix : OK
Short_Path (From_Dir => "c:\home\tests",
            To_File  => "c:\home\tests\mysite\site\d1\idx.txt") = mysite\site\d1\idx.txt

2. Dir with final / : OK
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = mysite/site/d1/idx.txt

3. subdir with Prefix : OK
Short_Path (From_Dir => "\home\tests",
            To_File  => "\home\tests\mysite\site\d1\idx.txt",
            Prefix   => ".\") = .\mysite\site\d1\idx.txt

4. Sibling subdir : NOK ****
Short_Path (From_Dir => "/home/tests/12/34",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = ..\..\mysite/site/d1/idx.txt
Expected ../../mysite/site/d1/idx.txt

5. Parent dir : NOK ****
Short_Path (From_Dir => "/home/tests/12/34",
            To_File  => "/home/tests/idx.txt") = ..\..\idx.txt
Expected ../../idx.txt

6. Other Prefix : NOK ****
Short_Path (From_Dir => "/home/tests/12/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt",
            Prefix   => "$PWD/") = $PWD/..\mysite/site/d1/idx.txt
Expected $PWD/../mysite/site/d1/idx.txt

# File_Utilities.Short_Path unit tests

# File_Utilities.Short_Path unit tests

# File_Utilities.Short_Path unit tests

1. Root dir : NOK ****
Short_Path (From_Dir => "/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = home/tests/mysite/site/d1/idx.txt
Expected /home/tests/mysite/site/d1/idx.txt

2. File is over dir : NOK ****
Short_Path (From_Dir => "/home/tests/mysite/site/d1",
            To_File  => "/home/readme.txt") = ..\..\..\..\readme.txt
Expected ../../../../readme.txt

3. File is over Dir, Dir with final / : NOK ****
Short_Path (From_Dir => "/home/tests/mysite/site/d1/",
            To_File  => "/home/readme.txt") = ..\..\..\..\readme.txt
Expected ../../../../readme.txt

4. File is the current dir : NOK ****
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests") = .\
Expected ./

5. File is over Dir, Dir and File with final / : NOK ****
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests/") = .\
Expected ./

6. No common part : NOK ****
Short_Path (From_Dir => "/home/toto/src/tests/",
            To_File  => "/opt/GNAT/2018/lib64/libgcc_s.so") = ..\..\..\..\opt/GNAT/2018/lib64/libgcc_s.so
Expected /opt/GNAT/2018/lib64/libgcc_s.so

7. Windows Path : OK
Short_Path (From_Dir => "c:\Users\Lionel\",
            To_File  => "c:\Users\Xavier\Proj") = ..\Xavier\Proj

8. Windows Path, case sensitivity : NOK ****
Short_Path (From_Dir => "c:\Users\Lionel\",
            To_File  => "c:\USERS\Xavier\Proj") = c:\USERS\Xavier\Proj
Expected ..\Xavier\Proj

9. UNC Path : NOK ****
Short_Path (From_Dir => "\\Volume\Server\Users\Lionel\",
            To_File  => "\\Volume\Server\USERS\Xavier\Proj") = ..\..\USERS\Xavier\Proj
Expected ..\Xavier\Proj


 8 tests fails
# File_Utilities.Short_Path unit tests

1. Root dir : NOK ****
Short_Path (From_Dir => "/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = home/tests/mysite/site/d1/idx.txt
Expected /home/tests/mysite/site/d1/idx.txt

# File_Utilities.Short_Path unit tests

1. Root dir : NOK ****
Short_Path (From_Dir => "/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = home/tests/mysite/site/d1/idx.txt
Expected /home/tests/mysite/site/d1/idx.txt

# File_Utilities.Short_Path unit tests

1. Root dir : OK
Short_Path (From_Dir => "/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = /home/tests/mysite/site/d1/idx.txt

# File_Utilities.Short_Path unit tests

1. Root dir : OK
Short_Path (From_Dir => "/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = /home/tests/mysite/site/d1/idx.txt

2. File is over dir : NOK ****
Short_Path (From_Dir => "/home/tests/mysite/site/d1",
            To_File  => "/home/readme.txt") = ..\..\..\..\readme.txt
Expected ../../../../readme.txt

3. File is over Dir, Dir with final / : NOK ****
Short_Path (From_Dir => "/home/tests/mysite/site/d1/",
            To_File  => "/home/readme.txt") = ..\..\..\..\readme.txt
Expected ../../../../readme.txt

4. File is the current dir : NOK ****
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests") = .\
Expected ./

5. File is over Dir, Dir and File with final / : NOK ****
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests/") = .\
Expected ./

6. No common part : OK
Short_Path (From_Dir => "/home/toto/src/tests/",
            To_File  => "/opt/GNAT/2018/lib64/libgcc_s.so") = /opt/GNAT/2018/lib64/libgcc_s.so

7. Windows Path : OK
Short_Path (From_Dir => "c:\Users\Lionel\",
            To_File  => "c:\Users\Xavier\Proj") = ..\Xavier\Proj

8. Windows Path, case sensitivity : NOK ****
Short_Path (From_Dir => "c:\Users\Lionel\",
            To_File  => "c:\USERS\Xavier\Proj") = c:\USERS\Xavier\Proj
Expected ..\Xavier\Proj

9. UNC Path : NOK ****
Short_Path (From_Dir => "\\Volume\Server\Users\Lionel\",
            To_File  => "\\Volume\Server\USERS\Xavier\Proj") = ..\..\USERS\Xavier\Proj
Expected ..\Xavier\Proj


 6 tests fails
# File_Utilities.Short_Path unit tests

1. Root dir : NOK ****
Short_Path (From_Dir => "d:",
            To_File  => "d:\home\tests\mysite\site\d1\idx.txt") = d:\home\tests\mysite\site\d1\idx.txt
Expected \home\tests\mysite\site\d1\idx.txt

2. File is over dir : NOK ****
Short_Path (From_Dir => "/home/tests/mysite/site/d1",
            To_File  => "/home/readme.txt") = ..\..\..\..\readme.txt
Expected ../../../../readme.txt

3. File is over Dir, Dir with final / : NOK ****
Short_Path (From_Dir => "/home/tests/mysite/site/d1/",
            To_File  => "/home/readme.txt") = ..\..\..\..\readme.txt
Expected ../../../../readme.txt

4. File is the current dir : NOK ****
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests") = .\
Expected ./

5. File is over Dir, Dir and File with final / : NOK ****
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests/") = .\
Expected ./

6. No common part : OK
Short_Path (From_Dir => "/home/toto/src/tests/",
            To_File  => "/opt/GNAT/2018/lib64/libgcc_s.so") = /opt/GNAT/2018/lib64/libgcc_s.so

7. Windows Path : OK
Short_Path (From_Dir => "c:\Users\Lionel\",
            To_File  => "c:\Users\Xavier\Proj") = ..\Xavier\Proj

8. Windows Path, case sensitivity : NOK ****
Short_Path (From_Dir => "c:\Users\Lionel\",
            To_File  => "c:\USERS\Xavier\Proj") = c:\USERS\Xavier\Proj
Expected ..\Xavier\Proj

9. UNC Path : NOK ****
Short_Path (From_Dir => "\\Volume\Server\Users\Lionel\",
            To_File  => "\\Volume\Server\USERS\Xavier\Proj") = ..\..\USERS\Xavier\Proj
Expected ..\Xavier\Proj


 7 tests fails
# File_Utilities.Short_Path unit tests

1. Root dir : NOK ****
Short_Path (From_Dir => "d:",
            To_File  => "d:\home\tests\mysite\site\d1\idx.txt") = d:\home\tests\mysite\site\d1\idx.txt
Expected \home\tests\mysite\site\d1\idx.txt

2. File is over dir : NOK ****
Short_Path (From_Dir => "/home/tests/mysite/site/d1",
            To_File  => "/home/readme.txt") = ..\..\..\..\readme.txt
Expected ../../../../readme.txt

3. File is over Dir, Dir with final / : NOK ****
Short_Path (From_Dir => "/home/tests/mysite/site/d1/",
            To_File  => "/home/readme.txt") = ..\..\..\..\readme.txt
Expected ../../../../readme.txt

4. File is the current dir : NOK ****
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests") = .\
Expected ./

5. File is over Dir, Dir and File with final / : NOK ****
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests/") = .\
Expected ./

6. No common part : OK
Short_Path (From_Dir => "/home/toto/src/tests/",
            To_File  => "/opt/GNAT/2018/lib64/libgcc_s.so") = /opt/GNAT/2018/lib64/libgcc_s.so

7. Windows Path : OK
Short_Path (From_Dir => "c:\Users\Lionel\",
            To_File  => "c:\Users\Xavier\Proj") = ..\Xavier\Proj

8. Windows Path, case sensitivity : NOK ****
Short_Path (From_Dir => "c:\Users\Lionel\",
            To_File  => "c:\USERS\Xavier\Proj") = c:\USERS\Xavier\Proj
Expected ..\Xavier\Proj

9. UNC Path : NOK ****
Short_Path (From_Dir => "\\Volume\Server\Users\Lionel\",
            To_File  => "\\Volume\Server\USERS\Xavier\Proj") = ..\..\USERS\Xavier\Proj
Expected ..\Xavier\Proj


 7 tests fails
# File_Utilities.Short_Path unit tests

1. Root dir : NOK ****
Short_Path (From_Dir => "d:",
            To_File  => "d:\home\tests\mysite\site\d1\idx.txt") = d:\home\tests\mysite\site\d1\idx.txt
Expected \home\tests\mysite\site\d1\idx.txt

2. File is over dir : OK
Short_Path (From_Dir => "/home/tests/mysite/site/d1",
            To_File  => "/home/readme.txt") = ..\..\..\..\readme.txt

3. File is over Dir, Dir with final / : NOK ****
Short_Path (From_Dir => "/home/tests/mysite/site/d1/",
            To_File  => "/home/readme.txt") = ..\..\..\..\readme.txt
Expected ../../../../readme.txt

4. File is the current dir : NOK ****
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests") = .\
Expected ./

5. File is over Dir, Dir and File with final / : NOK ****
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests/") = .\
Expected ./

6. No common part : OK
Short_Path (From_Dir => "/home/toto/src/tests/",
            To_File  => "/opt/GNAT/2018/lib64/libgcc_s.so") = /opt/GNAT/2018/lib64/libgcc_s.so

7. Windows Path : OK
Short_Path (From_Dir => "c:\Users\Lionel\",
            To_File  => "c:\Users\Xavier\Proj") = ..\Xavier\Proj

8. Windows Path, case sensitivity : NOK ****
Short_Path (From_Dir => "c:\Users\Lionel\",
            To_File  => "c:\USERS\Xavier\Proj") = c:\USERS\Xavier\Proj
Expected ..\Xavier\Proj

9. UNC Path : NOK ****
Short_Path (From_Dir => "\\Volume\Server\Users\Lionel\",
            To_File  => "\\Volume\Server\USERS\Xavier\Proj") = ..\..\USERS\Xavier\Proj
Expected ..\Xavier\Proj


 6 tests fails
# File_Utilities.Short_Path unit tests

1. Root dir : NOK ****
Short_Path (From_Dir => "d:",
            To_File  => "d:\home\tests\mysite\site\d1\idx.txt") = d:\home\tests\mysite\site\d1\idx.txt
Expected \home\tests\mysite\site\d1\idx.txt

2. File is over dir : OK
Short_Path (From_Dir => "/home/tests/mysite/site/d1",
            To_File  => "/home/readme.txt") = ..\..\..\..\readme.txt

3. File is over Dir, Dir with final / : OK
Short_Path (From_Dir => "/home/tests/mysite/site/d1/",
            To_File  => "/home/readme.txt") = ..\..\..\..\readme.txt

4. File is the current dir : OK
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests") = .\

5. File is over Dir, Dir and File with final / : NOK ****
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests/") = .\
Expected ./

6. No common part : OK
Short_Path (From_Dir => "/home/toto/src/tests/",
            To_File  => "/opt/GNAT/2018/lib64/libgcc_s.so") = /opt/GNAT/2018/lib64/libgcc_s.so

7. Windows Path : OK
Short_Path (From_Dir => "c:\Users\Lionel\",
            To_File  => "c:\Users\Xavier\Proj") = ..\Xavier\Proj

8. Windows Path, case sensitivity : NOK ****
Short_Path (From_Dir => "c:\Users\Lionel\",
            To_File  => "c:\USERS\Xavier\Proj") = c:\USERS\Xavier\Proj
Expected ..\Xavier\Proj

9. UNC Path : NOK ****
Short_Path (From_Dir => "\\Volume\Server\Users\Lionel\",
            To_File  => "\\Volume\Server\USERS\Xavier\Proj") = ..\..\USERS\Xavier\Proj
Expected ..\Xavier\Proj


 4 tests fails
