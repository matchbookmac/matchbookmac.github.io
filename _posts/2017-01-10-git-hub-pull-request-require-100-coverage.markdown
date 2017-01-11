---
layout: post
title:  "Github Pull Request: Require 100% Code Coverage"
date:   2017-01-10 11:40:03 -0800
categories: first post, github
---

Yesterday we got our API code up to 100% code coverage. For me, it's one of those things that once you get there, you
really don't want to lose it again. We use [Code Climate's](https://codeclimate.com) Github integration to ensure that
our coverage is at least above 90%, but as far as I know there is no way to configure that number to ensure that it
stays at 100%.

The CI/CD system that we use is [Atlassian Bamboo](https://www.atlassian.com/software/bamboo), and it allows for
arbitrary shell scripts to be run during a build. Currently we use that functionality to send updates to the Github API
to report success/failure on various build steps. For example:

``` shell
#!/bin/bash
# cribbed from http://broonix-rants.ghost.io/updating-github-status-from-bamboo/
cd ${bamboo.build.working.directory}

##check for test failures, or that they ran at all.
if test -e "api/test-reports/rspec.xml"; then
    if (grep 'failure message' api/test-reports/*.xml 1> /dev/null 2>&1) then
        mainJunit=$(find api/test-reports/*.xml -print0 | xargs -0 grep 'failure message' | wc -l)
        curl -s -H "Authorization: token <redacted>" --request POST --data '{"state": "failure", "context": "bamboo/tests", "description": "'"${mainJunit}"' Test(s) failed!", "target_url": "${bamboo.buildResultsUrl}"}' https://api.github.com/repos/<user>/<repo>/statuses/${bamboo.repository.revision.number} > /dev/null
    else
        curl -s -H "Authorization: token <redacted>" --request POST --data '{"state": "success", "context": "bamboo/tests", "description": "Tests Passed", "target_url": "${bamboo.buildResultsUrl}"}' https://api.github.com/repos/<user>/<repo>/statuses/${bamboo.repository.revision.number} > /dev/null
    fi
else
    curl -s -H "Authorization: token <redacted>" --request POST --data '{"state": "failure", "context": "bamboo/tests", "description": "Error running tests", "target_url": "${bamboo.buildResultsUrl}"}' https://api.github.com/repos/<user>/<repo>/statuses/${bamboo.repository.revision.number} > /dev/null
fi
```

I figured, it shouldn't be a far reach to do something similar for code coverage. So, I did some digging in the
`/coverage` directory of our project and saw: `.last_run.json`, and inside:

``` json
# coverage/.last_run.json
{
  "result": {
    "covered_percent": 100.0
  }
}
```

That might be useful!

Now, how to get the percentage out of the json? `jq` is great, but I didn't want to `apt-get` yet one more dependency
on our CI server; and, I thought it would be a good opportunity to get some practice with `grep` and `sed`. I came up
with this:

``` shell
grep -o '"covered_percent":.*[0-9]*' coverage/.last_run.json | sed 's/"covered_percent": //g' | sed 's/\.[0-9]*//g'
```

From the file above, this yields:

``` shell
100
```

Great!

What this does is, find lines with `"covered_percent"` and a number, delete the `"covered_percent": `, and delete the
decimal places (bash doesn't do floats, only integers). There may be a better way to do this, but it works! Now I needed
to integrate it into our build to notify Github whether we maintained 100% coverage or not. Using something like the
above, I came up with this:

``` shell
#!/bin/bash
coverage=$(grep -o '"covered_percent":.*[0-9]*' api/coverage/.last_run.json | sed 's/"covered_percent": //g' | sed 's/\.[0-9]*//g')
if [ $coverage -ge 100 ]; then
  curl -s -H "Authorization: token <redacted>" --request POST --data '{"state": "success", "context": "100% Code Coverage", "description": "Coverage == 100%", "target_url": "${bamboo.buildResultsUrl}"}' https://api.github.com/repos/<user>/<repo>/statuses/${bamboo.repository.revision.number} > /dev/null
else
  curl -s -H "Authorization: token <redacted>" --request POST --data '{"state": "failure", "context": "100% Code Coverage", "description": "Coverage < 100%", "target_url": "${bamboo.buildResultsUrl}"}' https://api.github.com/repos/<user>/<repo>/statuses/${bamboo.repository.revision.number} > /dev/null
fi
```

After completing a build with 100% code coverage, we were able to tell our Github repo to require the
`100% Code Coverage` status check, and now all subsequent pull requests will be required to meet 100% code coverage.
Success!
