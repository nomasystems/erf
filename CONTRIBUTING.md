# Contributing to erf

Any contribution is welcome. Here you have the guidelines you must follow to contribute to the project.

## Table of Contents  
[Doubts or questions](#doubts-or-questions)  
[Documentation](#documentation)  
[Bug reports](#bug-reports)  
[Feature requests](#feature-requests)  
[Development](#development)

## Doubts or questions

If you have any doubt or question about the project, please:

1. Read the [documentation](README.md) in case it helps you to clarify the concepts.
2. Check out [the issue tracker](https://github.com/nomasystems/erf/issues?q=is%3Aissue+label%3Aquestion). Maybe your question is already reflected there.
3. If none of the above makes it clearer for you, create a new question in the issue tracker. The team will try to answer as soon as possible.

## Documentation

Do you find that something is missing/wrong in the docs? Please, create a new documentation issue in the issue tracker. We will get back to you as soon as possible.

## Bug reports

Bug reports help us improve the code. We take them very seriously and thank you for taking the time to create them.

If you find a bug in the code, please:
 
1. Read the [documentation](README.md) to make sure you find an unexpected behaviour.
2. Check out [the issue tracker](https://github.com/nomasystems/erf/issues?q=is%3Aissue+label%3Abug). Maybe your bug report is already reflected there.
3. If you still think this is a new bug report, please create a new bug report in the issue tracker, and make sure you fill all the fields so the person solving the problem has all the necessary information.

## Feature requests

Feature requests are welcome. If you think that the project is missing a feature, please:
 
1. Read the [documentation](README.md) to make sure the feature is not already included.
2. Check out [the issue tracker](https://github.com/nomasystems/erf/issues?q=is%3Aissue+label%3Aenhancement). Maybe your feature was already requested by someone else.
3. If your feature is not reflected in any of the above, please create a new feature request in the issue tracker, giving as much context and detail as needed.

## Development

In order to develop something for the project, you must follow these rules:

1. Find an appropriate issue to solve:
   * Only verified issues with complete information must be addressed (avoid issues with label `needs triage`, as they must be reviewed by the nomasystems group).
   * If an issue is already assigned to another user, it means that this user is already solving the issue. Please, respect other people's work and don't interfere.
   * Issues labeled with `good first issue` are quicker and simpler, so they can be a good starting point when contributing.
2. Assign the issue to yourself, so other developers know that you are already working on it. Don't be ashamed if you find later that you won't be able to solve the issue, just left it unassigned so someone else can work on it.
3. Create a branch to solve the issue:
   * If the issue has the label `documentation`:
      - The new branch must be created from the branch named `develop`.
      - The name of the new branch must be `docs/{IssueIdentifier}-{IssueTitle in kebab-case}`.
   * If the issue has the label `bug`:
      - The new branch must be created from the branch named `main`.
      - The name of the new branch must be `fix/{IssueIdentifier}-{IssueTitle in kebab-case}`.
   * If the issue has the label `enhancement`:
      - The new branch must be created from the branch named `develop`.
      - The name of the new branch must be `feat/{IssueIdentifier}-{IssueTitle in kebab-case}`.
4. Make the changes in the new branch. Don't forget to:
   * Add new test cases checking that the bug is solved / the new feature is successfully implemented.
   * Use [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/) to create commit messages. If a commit closes an issue, the footer must include the expression `closes #IssueIdentifier`.
5. When the issue is solved, create a new pull request:
   * Add a description with the changes you've made.
   * Link the issue to the PR.
   * Left it unassigned, someone from the nomasystems group will review it as soon as possible.
