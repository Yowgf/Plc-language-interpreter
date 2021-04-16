cd /D "%~dp0"
cd ./src

if defined %%1 (sml start.sml ../%1) else sml start.sml