Fixme in current version
------------------------

Location | Text
---------|-----
[docs/features/B030_File_creation_in_Given_steps.md](../docs/features/B030_File_creation_in_Given_steps.md):14|> This last case is not yet tested because bbt doesn't support for now prompt interaction. ()  
[docs/features/B070_Mandatory_new_bug.md](../docs/features/B070_Mandatory_new_bug.md):1| bug 26 oct 2024 : the `Given the file whatever` is not overwriting an existing `whatever` file, even if it has not the same content.
[docs/proposed_features/contains_line.md](../docs/proposed_features/contains_line.md):5| not yet implemented.
[docs/UG.md](../docs/UG.md):172|>  as of 0.0.6, bbt is not able to simulate interactive behavior, and so this behavior is only partially tested.  
[src/bbt-cmd_line.adb](../src/bbt-cmd_line.adb):210|               --     --  opt -ot / --output_tag not yet coded
[src/bbt-cmd_line.ads](../src/bbt-cmd_line.ads):28|   procedure Create_Template; --  shouldn't be here
[src/bbt-main.adb](../src/bbt-main.adb):171|      --  what if there is multiple topics ont the cmd line?
[src/bbt-model-documents.ads](../src/bbt-model-documents.ads):48|     (D : in out Document_Type); --  should be private
[src/bbt-scenarios.ads](../src/bbt-scenarios.ads):55|   --  To be moved as dispatching in Writers
[src/bbt-scenarios-step_parser.adb](../src/bbt-scenarios-step_parser.adb):137|                                                                                                   --  we currently do not check if the existing file contains
[src/bbt-settings.ads](../src/bbt-settings.ads):82|   procedure Set_Result_File (File_Name : String); --  utile?
[src/bbt-tests-runner.adb](../src/bbt-tests-runner.adb):53|   --  Clearly not confortable with that function, it's magic.
[src/bbt-writers-markdown_writers.adb](../src/bbt-writers-markdown_writers.adb):172|      --  should be in parameters to avoid recomputing
[src/list_image-unix_predefined_styles.ads](../src/list_image-unix_predefined_styles.ads):55|   package Simple_One_Per_Line_Style is new Image_Style --  not the right name at all
