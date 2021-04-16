SET cur_dir=%CD%
SET input_file=%1

CD /D "%~dp0"
CD ./src

IF DEFINED input_file (
    sml start.sml %cur_dir%/%input_file%
) 
ELSE (
    sml start.sml
)