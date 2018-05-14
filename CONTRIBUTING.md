# Contributing to the GRiSP Project

:+1::tada: First off, thanks for taking the time to contribute! :tada::+1:

The following is a set of guidelines for contributing to the GRiSP project. These are just
guidelines, not rules, use your best judgment and feel free to propose changes
to this document in a pull request.

## What should I know before I get started?

### Code of Conduct

This project adheres to the Contributor Covenant
[code of conduct](CODE_OF_CONDUCT.md). By participating, you are expected to
uphold this code. Please report unacceptable behavior to
[grisp@stritzinger.com](mailto:grisp@stritzinger.com).

### Repositories

For the GRiSP project we have several repositories covering different aspects of the project. They are outlined below:

* [`grisp`](https://github.com/grisp/grisp)

  This is the repository for the GRiSP runtime that runs in the Erlang VM on the device itself. Issues that are related to runtime Erlang code running on the device should be reported to this repository. This includes issues with starting and running a release that the Rebar 3 plug-in built. Also issues related to the Erlang VM itself should be reported here, since the main part of customizations for Erlang and drivers used are in this repository.

* [`rebar3_grisp`](https://github.com/grisp/rebar3_grisp)

  This is a Rebar 3 plug-in that provides GRiSP related tasks, such as building the Erlang VM and deploying a GRiSP application onto a device. Issues related to using the plug-in such as errors when building or deploying should be reported here.
  
* [`grisp-software`](https://github.com/grisp/grisp-software)

  This is the toolchain, that provides RTEMS for GRiSP and contains all the tools (compilers etc.) used to build the Erlang VM running on the device. Low-level issues related to RTEMS or the toolchain should be reported here.

* [`otp`](https://github.com/grisp/otp)

  This is our fork of Erlang/OTP that contains GRiSP specific modifications to the build system necessary to get Erlang for GRiSP to build. Normally issues should be reported to one of the repositories above, unless you specifically know this is the right repository to report to.
  
Please take some time to figure out which is the correct repository to report to. When in doubt, report issues to the [`grisp`](https://github.com/grisp/grisp) repository.

## How Can I Contribute?

You can:

* Submit an issue (see [Reporting Bugs](#reporting-bugs))
* Suggest an enhancement (see [Suggesting Enhancements](#suggesting-enhancements))
* Submit a pull request (see [Pull Requests](#pull-requests))

### Your First Contribution

If you want to contribute with documentation, wiki pages, examples, code or
tests and are unsure of how to start, just open an issue to start a discussion.
It could be a proposal, a question for support or further direction or any
other feedback you might have.

### Reporting Bugs

Submitting a good bug report will help identifying, debugging and solving an
issue.

Please check first if any similar issues have already been reported. If so,
add to the discussion by commenting on one of those instead.

When you're ready to submit a bug report you can use the
[bug report template](.github/ISSUE_TEMPLATE.md) defined in the project (it's
automatically used whenever you create a new issue on GitHub). Make sure to
fill in which versions you are using and instructions of how to reproduce the
problem.

### Suggesting Enhancements

Suggesting enhancements doesn't have to be as structured as a bug report, but
should still contain the motivation for the enhancement, an example use case
and some reasoning why this enhancement should go into the project (a reason
for not including it can for example be that it can be implemented as an
external library).

### Pull Requests

Pull requests are greatly appreciated. To increase the chance of getting your code
merged, make sure the pull request is small and well structured. You should
prepare your pull request to try to meet the following guidelines where it
makes sense:

1. Squash all changes into one commit. If you have many independent changes,
   submit each in its own pull request.
2. Document any external API functions changed or added via EDoc.
3. Run the existing tests to make sure you didn't break anything.
3. Add working tests that illustrate and cover the changes, or detects an issue
   to be fixed. A good example is to create a failing test case that exposes the issue you are trying to fix, before fixing it.
4. Make sure the code and commit follow the [style guides](#styleguides).
5. (Optional) Add type specifications and run Dialyzer where it makes sense.

## Styleguides

### Commit Messages

Commit messages should be limited to 50 characters without a period on the
subject line and be written in imperative mood.

Longer explanations should be in the body, two lines below the message, wrapped at 72 characters.

See [How to Write a Git Commit Message](http://chris.beams.io/posts/git-commit/).

### Code

* Lines should be no longer than 80 characters. This is isn't some arbitrary
  length based on nostalgia, it's just a choice of fitting limit if you want
  to have several files open at once next to each other on a modern wide screen
  monitor.
* Functions should be exported one by one in their own export statement. This
  is so that one export can easily be rearranged or removed without messing
  with commas and Erlang attributes.

If you are unsure, try to find some similar code already in the repository and
mimic that. Otherwise just submit the pull request to get some stylistic
feedback if necessary.
