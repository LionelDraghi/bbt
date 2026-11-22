# bbt specifications <!-- omit from toc -->

This document is the index of the bbt feature pages. The feature pages
own every detail; this index may summarise, never duplicate.

The numbering follows the template [specifications page]
(https://github.com/LionelDraghi/ada-project-template/blob/main/project/docs/specifications.md):
one series prefix per feature domain, a number never changes meaning,
and a retired number is never reused.

| Series | Domain |
|---|---|
| `Annn` | Step vocabulary and matching (CLI) |
| `Bnnn` | Harness behavior: files, directories, options, output |
| `Cnnn` | Error handling and diagnostics |
| `Gnnn` | GUI vocabulary |

## 1. Feature index

### `Annn` — Step vocabulary and matching

| Id | Feature page |
|---|---|
| A005 | [cli_features/A005_Given.md](cli_features/A005_Given.md) |
| A006 | [cli_features/A006_Given_Executable_File.md](cli_features/A006_Given_Executable_File.md) |
| A010 | [cli_features/A010_Then_Contains_Code_Span.md](cli_features/A010_Then_Contains_Code_Span.md) |
| A020 | [cli_features/A020_Then_Contains_Code_Block.md](cli_features/A020_Then_Contains_Code_Block.md) |
| A030 | [cli_features/A030_Then_Does_Not_Contain.md](cli_features/A030_Then_Does_Not_Contain.md) |
| A040 | [cli_features/A040_Example_Keyword.md](cli_features/A040_Example_Keyword.md) |
| A050 | [cli_features/A050_Then_File_Is_Code_Block.md](cli_features/A050_Then_File_Is_Code_Block.md) |
| A060 | [cli_features/A060_Then_File_Equal_Code_Span.md](cli_features/A060_Then_File_Equal_Code_Span.md) |
| A065 | [cli_features/A065_Then_File_Equal_File.md](cli_features/A065_Then_File_Equal_File.md) |
| A070 | [cli_features/A070_Then_Get_Code_Span.md](cli_features/A070_Then_Get_Code_Span.md) |
| A080 | [cli_features/A080_Then_Output_Equal_To_File.md](cli_features/A080_Then_Output_Equal_To_File.md) |
| A090 | [cli_features/A090_Then_Get_Error.md](cli_features/A090_Then_Get_Error.md) |
| A100 | [cli_features/A100_Then_Get_Stderr.md](cli_features/A100_Then_Get_Stderr.md) |
| A110 | [cli_features/A110_Then_No_Output.md](cli_features/A110_Then_No_Output.md) |
| A120 | [cli_features/A120_Then_Get_String.md](cli_features/A120_Then_Get_String.md) |
| A130 | [cli_features/A130_Successfully_Keyword.md](cli_features/A130_Successfully_Keyword.md) |
| A140 | [cli_features/A140_Unordered_Keyword.md](cli_features/A140_Unordered_Keyword.md) |
| A150 | [cli_features/A150_Background_Keyword.md](cli_features/A150_Background_Keyword.md) |
| A160 | [cli_features/A160_Ignoring_Blank_Lines.md](cli_features/A160_Ignoring_Blank_Lines.md) |
| A170 | [cli_features/A170_File_vs_File_Name.md](cli_features/A170_File_vs_File_Name.md) |
| A190 | [cli_features/A190_Run.md](cli_features/A190_Run.md) |
| A195 | [cli_features/A195_Run_Or.md](cli_features/A195_Run_Or.md) |
| A200 | [cli_features/A200_Regexp.md](cli_features/A200_Regexp.md) |
| A210 | [cli_features/A210_Exact_Match.md](cli_features/A210_Exact_Match.md) |
| A220 | [cli_features/A220_AsciiDoc_gcc_hello_world.adoc](cli_features/A220_AsciiDoc_gcc_hello_world.adoc) |
| A230 | [cli_features/A230_select_exclude_include.md](cli_features/A230_select_exclude_include.md) |
| A240 | [cli_features/A240_Then_I_Succesfully_Run.md](cli_features/A240_Then_I_Succesfully_Run.md) |
| A250 | [cli_features/A250_Fails.md](cli_features/A250_Fails.md) |
| A260 | [cli_features/A260_Then_Exit_Code_Is.md](cli_features/A260_Then_Exit_Code_Is.md) |
| A270 | [cli_features/A270_Then_Error_Output.md](cli_features/A270_Then_Error_Output.md) |
| A280 | [cli_features/A280_Given_Environment_Variable.md](cli_features/A280_Given_Environment_Variable.md) |
| A290 | [cli_features/A290_When_I_Type_Or_Enter.md](cli_features/A290_When_I_Type_Or_Enter.md) |
| A300 | [cli_features/A300_Ignoring.md](cli_features/A300_Ignoring.md) |
| A310 | [cli_features/A310_Run_Shell.md](cli_features/A310_Run_Shell.md) |

### `Bnnn` — Harness behavior

| Id | Feature page |
|---|---|
| B010 | [cli_features/B010_Deleting_created_files.md](cli_features/B010_Deleting_created_files.md) |
| B020 | [cli_features/B020_Exec_dir.md](cli_features/B020_Exec_dir.md) |
| B030 | [cli_features/B030_File_creation_in_Given_steps.md](cli_features/B030_File_creation_in_Given_steps.md) |
| B040 | [cli_features/B040_Find_scenarios.md](cli_features/B040_Find_scenarios.md) |
| B050 | [cli_features/B050_Strict_gherkin.md](cli_features/B050_Strict_gherkin.md) |
| B060 | [cli_features/B060_Return_code_on_test_failure.md](cli_features/B060_Return_code_on_test_failure.md) |
| B080 | [cli_features/B080_Keep_Going.md](cli_features/B080_Keep_Going.md) |
| B090 | [cli_features/B090_tmp_dir.md](cli_features/B090_tmp_dir.md) |
| B100 | [cli_features/B100_Results_Output_In_MD_Format.md](cli_features/B100_Results_Output_In_MD_Format.md) |
| B105 | [cli_features/B105_Expected_vs_Actual_Output.md](cli_features/B105_Expected_vs_Actual_Output.md) |
| B110 | [cli_features/B110_Spawn.md](cli_features/B110_Spawn.md) |
| B115 | [cli_features/B115_Warning_On_Shell_Metacharacters.md](cli_features/B115_Warning_On_Shell_Metacharacters.md) |
| B120 | [cli_features/B120_Output_Verbosity.md](cli_features/B120_Output_Verbosity.md) |
| B130 | [cli_features/B130_Cmd_Line_Help.md](cli_features/B130_Cmd_Line_Help.md) |
| B140 | [cli_features/B140_Index_File.md](cli_features/B140_Index_File.md) |
| B150 | [cli_features/B150_Deprecated_Options.md](cli_features/B150_Deprecated_Options.md) |
| B160 | [cli_features/B160_JUnit_XML_Export.md](cli_features/B160_JUnit_XML_Export.md) |
| B170 | [cli_features/B170_Explain.md](cli_features/B170_Explain.md) |
| B180 | [cli_features/B180_Ignored_Files.md](cli_features/B180_Ignored_Files.md) |
| B190 | [cli_features/B190_Clean_Error_When_No_Output.md](cli_features/B190_Clean_Error_When_No_Output.md) |
| B200 | [cli_features/B200_Scenario_Timeout.md](cli_features/B200_Scenario_Timeout.md) |
| B210 | [cli_features/B210_Status_Bar.md](cli_features/B210_Status_Bar.md) |

### `Cnnn` — Error handling and diagnostics

| Id | Feature page |
|---|---|
| C010 | [cli_features/C010_Empty_scenarios.md](cli_features/C010_Empty_scenarios.md) |
| C030 | [cli_features/C030_Markdown_syntax.md](cli_features/C030_Markdown_syntax.md) |
| C040 | [cli_features/C040_Missing_title.md](cli_features/C040_Missing_title.md) |
| C050 | [cli_features/C050_Step_marker.md](cli_features/C050_Step_marker.md) |
| C060 | [cli_features/C060_code_block.md](cli_features/C060_code_block.md) |
| C070 | [cli_features/C070_missing_code_block.md](cli_features/C070_missing_code_block.md) |
| C080 | [cli_features/C080_missing_scenario.md](cli_features/C080_missing_scenario.md) |
| C090 | [cli_features/C090_GNU_Error_Msg_Format.md](cli_features/C090_GNU_Error_Msg_Format.md) |
| C110 | [cli_features/C110_Two_Verbs_In_The_Step.md](cli_features/C110_Two_Verbs_In_The_Step.md) |
| C120 | [cli_features/C120_Ill_Formated_Steps.md](cli_features/C120_Ill_Formated_Steps.md) |

### `Gnnn` — GUI vocabulary

| Id | Feature page |
|---|---|
| G005 | [G005 — Launch and window observation](gui_features.md#g005--launch-and-window-observation) |
| G010 | [G010 — Text content of a window](gui_features.md#g010--text-content-of-a-window) |
| G015 | [G015 — Button by accessible name, with enabled state](gui_features.md#g015--button-by-accessible-name-with-enabled-state) |
| G020 | [G020 — Press action on a control](gui_features.md#g020--press-action-on-a-control) |
| G035 | [G035 — Type text into a field, and enter it](gui_features.md#g035--type-text-into-a-field-and-enter-it) |
| G080 | [G080 — List structure, with content and emptiness checks](gui_features.md#g080--list-structure-with-content-and-emptiness-checks) |
| G095 | [G095 — Window close and cross-scenario application lifetime](gui_features.md#g095--window-close-and-cross-scenario-application-lifetime) |

## 2. Proposed features

Candidates pending absorption. On absorption, a proposed feature
gets its entry in [gui_features.md](gui_features.md), its
executable scenarios in the shared witness suite, and its rows in
the [backends.md](backends.md) cross tables. A number never changes
meaning between the proposed page and the absorbed one.
Several candidates may share one user story scenario: the story
exercises them together on a single application launch.

| Id | Proposed feature | Milestone | Status |
|---|---|---|---|
| G025 | [G025_dialog_eventually.md](proposed/G025_dialog_eventually.md) | 1 — minimal loop (Web) | pending vocabulary |
| G030 | [G030_text_fields_focus.md](proposed/G030_text_fields_focus.md) | 1 — minimal loop (Web) | pending vocabulary |
| G040 | [G040_alert_status.md](proposed/G040_alert_status.md) | 1 — minimal loop (Web) | pending vocabulary |
| G045 | [G045_widget_states.md](proposed/G045_widget_states.md) | 2 — wider ARIA vocabulary (Web) | not started |
| G050 | [G050_mvp_widget_roles.md](proposed/G050_mvp_widget_roles.md) | 2 — wider ARIA vocabulary (Web) | not started |
| G055 | [G055_structure_landmarks.md](proposed/G055_structure_landmarks.md) | 2 — wider ARIA vocabulary (Web) | not started |
| G060 | [G060_focus_navigation.md](proposed/G060_focus_navigation.md) | 2 — wider ARIA vocabulary (Web) | not started |
| G065 | [G065_drag_and_drop.md](proposed/G065_drag_and_drop.md) | 3 — beyond the reference scenarios | gated |
| G070 | [G070_region_snapshots.md](proposed/G070_region_snapshots.md) | 3 — beyond the reference scenarios | gated |
| G085 | [G085_slider.md](proposed/G085_slider.md) | 2 — wider ARIA vocabulary (Web) | pending vocabulary |
| G090 | [G090_spinbutton.md](proposed/G090_spinbutton.md) | 2 — wider ARIA vocabulary (Web) | pending vocabulary |

## 3. Cross-feature specifications

Each specification carries a stable identifier `Snnn`. A number never
changes meaning, and a removed specification is never reused.

### S001 — Action provenance

*Source: `docs/proposed/candidate-features.md` (cross-cutting
injection levels); `docs/dev/ci-strategy.md`.*

The same scenario line can be executed at different injection
levels, with different fidelity — a semantic action (the
accessibility API does the gesture, e.g. an AT-SPI DoAction), a
toolkit input, a system input (virtual keyboard or mouse, e.g. the
`Return` synthesis of G035), or a direct manipulation. The
architecture makes end-to-end fidelity explicit, and the CI
strategy requires a capability report to state the injection level
used: this must be visible, not implicit.

No scenario vocabulary: every executed action carries a recorded
provenance, exposed in reports and diagnostics — a backend that
silently degrades from one level to another is detectable. This
ties the action-tier model (G020, G060) to the capability report
and the CI strategy profiles
([ARIA-AT](https://github.com/w3c/aria-at) is the model for results
consolidated per platform).

Open questions: Q008.

Next free number: **S002**.
