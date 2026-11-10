# Proposal: Responsive UI Testing and Viewport Vocabulary

## Status

Proposed

## Context

GuiVerb is being merged into BBT, bringing behaviour-driven GUI testing into the
general BBT scenario model.

The initial GuiVerb vocabulary is largely inspired by WAI-ARIA:

- accessible roles such as `button`, `tree`, `textbox`, etc.;
- accessible names;
- states and properties;
- user actions on accessible UI elements.

This provides a portable vocabulary for describing what an interface exposes to
the user, independently of the underlying GUI toolkit.

However, some GUI behaviours depend not only on the accessible interface but also
on the environment in which that interface is rendered.

Responsive interfaces are an important example.

A Web application may expose a different layout depending on the available
viewport:

- navigation may be permanently visible on a large screen;
- the same navigation may be hidden behind a menu on a small screen;
- secondary panels may disappear;
- components may move relative to each other;
- functionality may remain available through a different interaction path.

ARIA does not define the concept of a viewport. Viewports belong to the Web/CSS
rendering model, notably CSSOM View.

GuiVerb/BBT therefore needs a small vocabulary describing the rendering
environment in addition to the ARIA-derived vocabulary describing the interface.

## Proposal

Introduce viewport-related preconditions in the GUI vocabulary.

For example:

    Given the viewport is 390 x 844

or, when only one dimension matters:

    Given the viewport width is 390

These steps describe the environment in which the scenario is executed rather
than an element of the interface itself.

The backend is responsible for translating this request into the appropriate
operation.

For a Web backend this could mean resizing the browser viewport.

Other GUI backends could interpret the same concept as resizing the application
window or its available rendering area, when applicable.

## Step and State Semantics

Like the other environment settings (e.g. environment variables),
the viewport is scenario state: it is reset to its default at each
scenario start, so a `Given the viewport ...` step applies to the
whole scenario that issues it, and only to it.

## Example

### Common behaviour

Some properties should hold independently of the viewport.

    Scenario: The editor is available
      Then there is a text zone "Editor"
      And there is a button "Run"
      And there is a button "Open"

These scenarios constitute the invariant functional contract of the interface.

### Small viewport

    Scenario: Navigation adapts to a small viewport
      Given the viewport is 390 x 844

      Then there is a text zone "Editor"
      And there is no tree "Project"
      And there is a button "Menu"

      When I activate "Menu"
      Then there is a tree "Project"

This describes an important behavioural property: functionality has not
disappeared on the small screen; its access path has changed.

### Large viewport

    Scenario: Navigation is directly available on a large viewport
      Given the viewport is 1440 x 900

      Then there is a tree "Project"
      And there is a text zone "Editor"
      And the tree "Project" is left of the text zone "Editor"

## Layout Breakpoints

Note: layout breakpoints, as in CSS media queries, are unrelated to
the debugger breakpoints of the interactive execution proposal.

Responsive behaviour should preferably be tested in terms of viewport dimensions,
rather than device names.

For example, if an application changes layout at 768 CSS pixels:

    Scenario: Compact navigation below the breakpoint
      Given the viewport width is 767
      Then there is no tree "Project"
      And there is a button "Menu"

    Scenario: Full navigation at the breakpoint
      Given the viewport width is 768
      Then there is a tree "Project"

This directly tests the responsive contract and avoids assumptions associated
with labels such as "phone", "tablet", or "desktop".

Device presets may later be introduced as convenience features, but should not
replace explicit viewport dimensions in the core vocabulary.

## Scenario Organization

Responsive behaviour should normally be organized through separate scenarios or
feature files.

For example:

    responsive/
        common.md
        mobile.md
        desktop.md

`common.md` describes properties that must always hold.

`mobile.md` describes the compact form of the interface.

`desktop.md` describes the expanded form.

The names are organizational conventions; the scenarios themselves should rely
on explicit viewport dimensions.

## Why Not Conditional Steps?

An alternative syntax could be:

    Then there is a tree "Project" @big_screen_only
    And there is a text zone "Editor"
    And there is a button "Menu" @small_screen_only

This proposal deliberately avoids such conditional steps.

A BBT scenario should describe one coherent observable state of the system.

Introducing conditions at the step level would progressively turn scenarios into
a form of conditional program:

    @desktop_only
    @mobile_only
    @touch_only
    @landscape_only
    @dark_mode_only
    ...

This would make scenarios harder to read as specifications and documentation.

If tags are needed for test selection, they should apply to scenarios or features,
not alter the meaning of individual steps.

## Vocabulary Model

This proposal introduces a useful distinction in the GuiVerb/BBT vocabulary.

### Interface vocabulary

Primarily derived from accessibility semantics:

    button
    tree
    text zone
    checkbox
    selected
    checked
    expanded
    ...

It describes **what the user can perceive and interact with**.

### Environment vocabulary

Describes the conditions under which the interface is presented:

    Given the viewport is 390 x 844
    Given the viewport width is 767

Future extensions could potentially include:

    Given the color scheme is dark
    Given the interface is in landscape orientation
    Given the input device is touch

These concepts should only be added when they express observable behavioural
requirements that cannot be adequately represented by the ARIA-derived
vocabulary.

## Scope

The goal is behavioural responsive testing, not CSS conformance testing.

BBT should test properties such as:

- whether functionality remains accessible;
- whether elements appear or disappear according to available space;
- whether navigation changes mode;
- whether important components remain reachable;
- whether the relative organization of significant UI elements changes.

BBT should not attempt to become a CSS assertion framework.

Assertions such as the following are deliberately outside the primary vocabulary:

    margin-left == 16px
    font-size == 14px
    width == 327px

Pixel-perfect rendering is better handled by visual regression tools.

## Design Principle

The resulting model is:

    environment + accessible semantics + behaviour

ARIA-inspired vocabulary describes the interface.

Environment steps such as `Given the viewport is ...` establish the conditions
under which that interface is observed.

This keeps scenarios readable as behavioural specifications while allowing BBT
to test responsive interfaces without introducing CSS-specific implementation
details.

## Initial Vocabulary Extension

The first implementation can deliberately remain minimal:

    Given the viewport is <width> x <height>
    Given the viewport width is <width>
    Given the viewport height is <height>

Further environment vocabulary should only be introduced from concrete testing
requirements.

## Open Questions

- Are viewport dimensions always expressed in CSS pixels?
  Presumably yes for Web backends (device independent, physical
  pixels excluded), and logical pixels for the other backends:
  to be confirmed.
- Should `viewport` map to browser viewport or application window for non-Web
  backends?
  Probably the rendering area (the client area, without window
  decorations), the equivalent of the CSS viewport: to be confirmed.
- Should orientation be derived from width/height rather than introduced as a
  separate primitive?
  Probably yes, to keep the vocabulary minimal.
- Should viewport presets such as `mobile` or `desktop` exist only in
  configuration rather than in the scenario language?
  Probably yes, like the organizational conventions above.
- What should a backend do when it cannot control the viewport?
  (undecided)
- Should resizing be an observable action (e.g. `When I resize the viewport
  to 800 x 600`) to test the responsive transition itself, rather than a
  precondition only? (undecided)