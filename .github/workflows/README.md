# Manually trigger OTP package generation

Here is a quick example with the GitHub CLI, for more info refer to the [docs](https://docs.github.com/en/rest/repos/repos?apiVersion=2022-11-28#create-a-repository-dispatch-event).
You can do this whenever you see we are missing one or more releases, you can pass multiple versions in the list.

```shell
gh api \
  --method POST \
  -H "Accept: application/vnd.github+json" \
  -H "X-GitHub-Api-Version: 2022-11-28" \
  /repos/grisp/grisp/dispatches \
  -f "event_type=new-otp-release" \
  -F "client_payload[otp]=[\\\"27.0\\\", \\\"26.2.5.1\\\"]" \
  -F "client_payload[unit]=false" \
  -F "client_payload[integration]=true"
```
