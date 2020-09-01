git clone https://github.com/microsoft/vcpkg
call .\vcpkg\bootstrap-vcpkg.bat
vcpkg\vcpkg install getopt:x64-windows
setx /M PATH "%PATH$%;%cd%\vcpkg\installed\x64-windows\bin"
setx /M GETOPT_INCLUDE "%INCLUDE%;%cd%\vcpkg\installed\x64-windows\include;"
setx /M GETOPT_LIB "%LIB%;%cd%\vcpkg\installed\x64-windows\lib"
call refreshenv
echo "HELLO?"
echo "PATH: %PATH%"
echo "INCLUDE: %GETOPT_INCLUDE%"
echo "LIB: %GETOPT_LIB%"
dir C:\Users\travis\build\brianfairservice\souffle\vcpkg\installed\x64-windows\lib
