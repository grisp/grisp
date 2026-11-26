# Release instructions

## Before a release

1. Make sure that [release.yml](.github/workflows/release.yml) is on the default branch.
2. Use a GitHub CLI account with write access to `grisp/grisp`.
3. Make sure that CI passed for the commit on the default branch.
4. Make sure that Hex Workload Identity uses `grisp/grisp`, `release.yml`, and the `release` environment.

The GitHub `release` environment supplies the `RELEASE_SIGNING_KEY` secret and the `RELEASE_SIGNING_EMAIL` variable.
Replace the signing secret placeholders with the release key and its passphrase
before the first release. Use an empty passphrase for an unprotected key.
GitHub must verify the key for the signing email because `master` requires
verified commits.

## Send a release request

1. Replace `X.Y.Z` with the version that the user gives.
2. Run the following command.

   ```sh
   gh api --method POST repos/grisp/grisp/dispatches --input - <<'JSON'
   {"event_type":"release","client_payload":{"version":"X.Y.Z"}}
   JSON
   ```

3. Open the new **Release** run in [GitHub Actions](https://github.com/grisp/grisp/actions/workflows/release.yml).
4. Get the Hex approval link from the **Publish to Hex** step log. For a docs
   upload by itself, use the **Publish the Hex documentation** step log.
5. Give the link to the person who can approve the release on Hex.

The person who approves the release must complete two-factor authentication on Hex.
The workflow creates the GitHub release before staging the package and documentation on Hex.
The job ends after the upload succeeds. Hex approval can happen later.

## Continue a release

If Hex approval is pending after a successful run, use the link in the
completed run to approve it. Approval completes the release on Hex.

If the run fails after pushing the tag, correct the cause and select
**Re-run all jobs** on the original run. The workflow checks that the tag
belongs to that run.

## Generate prebuilt OTP packages

After approving the GRiSP release on Hex, dispatch the existing OTP package
workflow for the latest patch release in each of the four most recent major
OTP series, starting at OTP 27. Use the [OTP versions table] and the same
selection as the [GitHub release handler](.github/workflows/on_grisp_release.yaml).

[OTP versions table]: https://raw.githubusercontent.com/erlang/otp/master/otp_versions.table

```sh
gh api --method POST repos/grisp/grisp/dispatches --input - <<'JSON'
{"event_type":"grisp-release","client_payload":{"otp":"OTP_VERSION","unit":false,"integration":true}}
JSON
```

Replace `OTP_VERSION` with a version such as `29.0.6`. Monitor the
[OTP package runs](https://github.com/grisp/grisp/actions/workflows/otp_pkg_on_grisp_release.yaml).
Releases created by the workflow's `GITHUB_TOKEN` skip the automatic
`release: published` trigger.
