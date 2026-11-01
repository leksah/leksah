// Canned agents for the in-browser demo's Agents pane (side bar, ⌥⌘2).
// Read once by IDE.Web.AgentInfo's ghcjs branch of agentForest; the shape
// mirrors AgentNode (children nest the same shape, "pr" is [number, url]).
// "name" must be one of IDE.Web.AgentNames' animals (it picks the face);
// "activity" is the card's what-it-is-doing-now line; "bio" feeds the Team
// pane.
// Keep states to busy/waiting/idle: rows are display-only in the demo (their
// click/⟳ actions find no pane and return silently), but a "gone" row's
// click path would try to spawn a resume.  Descriptions are sanitized on
// the way in (whitelist tags; href becomes an inert data-href the pane's
// link handler routes), so plain <a href> links are fine here.
window.leksahDemoAgents = [
  { session: "d3adbeef-0001-4a5b-8c6d-breakout01",
    name: "Otter",
    title: "Add power-ups to breakout",
    state: "busy",
    activity: "Editing src/PowerUp.hs",
    detail: "Working — implementing falling power-up bricks",
    dir: "/demo/breakout",
    branch: "feature/power-ups",
    pr: [412, "https://github.com/leksah/leksah/pull/412"],
    desc: "Multi-ball and paddle-grow power-ups drop from bricks. " +
          "Physics done; tuning drop rates. " +
          "<a href=\"https://github.com/leksah/leksah/pull/412\">PR #412</a> is up, CI green.",
    bio: "Gameplay programmer. Likes small, testable physics functions and " +
         "hands anything test-shaped to Wren.",
    children: [
      { session: "d3adbeef-0002-4a5b-8c6d-breakout02",
        name: "Wren",
        title: "Write collision tests",
        state: "idle",
        detail: "Idle — waiting for your reply",
        dir: "/demo/breakout",
        branch: "feature/power-ups",
        desc: "Property tests for <code>overlaps</code> and the paddle bounce " +
              "angle. 14 tests passing; ready for review.",
        bio: "Started by Otter to write property tests; reports back when " +
             "they pass." }
    ] },
  { session: "d3adbeef-0003-4a5b-8c6d-breakout03",
    name: "Heron",
    title: "Level editor design",
    state: "waiting",
    activity: "Waiting to run: mkdir levels",
    detail: "Needs you — blocked on a permission prompt",
    dir: "/demo/breakout",
    desc: "Sketching a text format for level layouts. " +
          "Waiting for approval to add the <code>levels/</code> directory.",
    bio: "Designs file formats and tools; prefers plain text you can diff." }
];
