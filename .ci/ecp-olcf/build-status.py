#!/usr/bin/env python3
# Adapted from from ECP-CI documentation
# https://ecp-ci.gitlab.io/docs/guides/build-status-gitlab.html

import json
import os
import requests

# In order to improve the readability of this example we've seperated
# obtaining the target environment variables from the request.
# Note that any variables prefixed by "BUILDSTATUS_" are established
# by the project maintainer, else they are provided by the runner.

owner = os.getenv('BUILDSTATUS_OWNER')
project = os.getenv('BUILDSTATUS_PROJECT')
api = os.getenv('BUILDSTATUS_APIURL')
token = os.getenv('BUILDSTATUS_TOKEN')
name = os.getenv('BUILDSTATUS_JOB')

sha = os.getenv('CI_COMMIT_SHA')
state = os.getenv('CI_JOB_NAME')
url = os.getenv('CI_PIPELINE_URL')

# https://developer.github.com/v3/repos/statuses/
# Load and identify requirement from environment.
status = {
    'state': state,
    'target_url': url,
    'context': name
}

r = requests.post("{}/repos/{}/{}/statuses/{}".format(api, owner, project, sha),
                headers={'Authorization': 'token {}'.format(token)},
                data=json.dumps(status))
if r.status_code != 201:
    print(r.text)
    exit(1)
