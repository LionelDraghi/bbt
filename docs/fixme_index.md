Fixme in current version
------------------------

Location | Text
---------|-----
[docs/features/B030_File_creation_in_Given_steps.md](../docs/features/B030_File_creation_in_Given_steps.md):14|> This last case is not yet tested because bbt doesn't support for now prompt interaction. ()  
[docs/features/B130_Cmd_Line_Help.md](../docs/features/B130_Cmd_Line_Help.md):153| but I don't know how to test it!
[docs/proposed_features/B070_Mandatory_new_bug.md](../docs/proposed_features/B070_Mandatory_new_bug.md):1| bug 26 oct 2024 : the `Given the file whatever` is not overwriting an existing `whatever` file, even if it has not the same content.
[docs/proposed_features/error_output_contains.md](../docs/proposed_features/error_output_contains.md):3| not yet implemented.
[docs/UG.md](../docs/UG.md):174|>  as of 0.0.6, bbt is not able to simulate interactive behavior, and so this behavior is only partially tested.  
[src/bbt-cmd_line.adb](../src/bbt-cmd_line.adb):207|               --     --  opt -ot / --output_tag not yet coded
[src/bbt-model-documents.adb](../src/bbt-model-documents.adb):211|      --  to be replaced with a Reduce?
[src/bbt-model-documents.ads](../src/bbt-model-documents.ads):49|     (D : in out Document_Type); --  should be private
[src/bbt-model-documents.ads](../src/bbt-model-documents.ads):66|     (D : in out Documents_Lists.Vector) --  should be type List
[src/bbt-scenarios.ads](../src/bbt-scenarios.ads):58|   --  To be moved as dispatching in Writers
[src/bbt-scenarios-files.adb](../src/bbt-scenarios-files.adb):194|            --  to be moved in Text_Utilities or so
[src/bbt-scenarios-steps.adb](../src/bbt-scenarios-steps.adb):274|               when Unknown    => null; --  should be an explicit error message?
[src/bbt-scenarios-steps-initialize_grammar.adb](../src/bbt-scenarios-steps-initialize_grammar.adb):35|                                                                                         --  we currently do not check if the existing file contains
[src/bbt-scenarios-steps-validate_step_state.adb](../src/bbt-scenarios-steps-validate_step_state.adb):16|   -- To move in Text_Utilities
[src/bbt-tests-runner.adb](../src/bbt-tests-runner.adb):53|   --  Clearly not confortable with that function, it's magic.
[src/bbt-tests-runner.adb](../src/bbt-tests-runner.adb):89|      --  defensive code that should be replaced by
[src/bbt-writers-markdown_writers.adb](../src/bbt-writers-markdown_writers.adb):160|      --  Path_To_Scen should be in Scenario_Type to avoid
[src/bbt-writers-markdown_writers.adb](../src/bbt-writers-markdown_writers.adb):210|      --  Path_To_Scen should be in Scenario_Type to avoid recomputing
[src/list_image-unix_predefined_styles.ads](../src/list_image-unix_predefined_styles.ads):55|   package Simple_One_Per_Line_Style is new Image_Style --  not the right name at all
