Fixme in current version
------------------------

Location | Text
---------|-----
docs/features/A190_Run.md:40| this scenario fail on Windows : on Windows, Spawn return Success 
docs/features/B030_File_test_and_creation.md:14|> This last case is not yet tested because bbt doesn't support for now prompt interaction. ()  
docs/features/B040_Find_scenarios.md:64| this scenario fail on Windows because of the directory separator in the expected result.  
docs/features/B040_Find_scenarios.md:87| this scenario fail on Windows because of the directory separator in the expected result.  
docs/features/B070_Mandatory_new_bug.md:1| bug 26 oct 2024 : the `Given the file whatever` is not overwriting an existing `whatever` file, even if it has not the same content.
docs/features/B090_tmp_dir.md:30| dir2 is not removed although --cleanup
docs/features/B110_Spawn.md:9| this scenario is Unix specific, and should not be run on Windows.  
docs/features/proposed_features/contains_line.md:5| not yet implemented.
docs/UG.md:175|>  as of 0.0.6, bbt is not able to simulate interactive behavior, and so this behavior is only partially tested.  
src/bbt-main-analyze_cmd_line.adb:81|         --     --  opt -ot / --output_tag not yet coded
src/bbt-scenarios-step_parser.adb:131|                                                                                                   --  we currently do not check if the existing file contains
src/bbt-tests-runner.adb:30|   --  Clearly not confortable with that function, it's magic.
src/list_image-unix_predefined_styles.ads:55|   package Simple_One_Per_Line_Style is new Image_Style --  not the right name at all
