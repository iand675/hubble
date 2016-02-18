module GitHub.Repositories.Statistics where

getContributors
  :: GitHubM s m
  => Owner
  -> RepoName
  -> m (GitHubResponse (Vector Contributor))
getContributorsList (Owner o) (RepoName r)
  = get (v3 json) ["repos", o, r, "stats", "contributors"] []

getCodeFrequency
  :: GitHubM s m
  => Owner
  -> RepoName
  -> m (GitHubResponse (Vector CodeFrequency))
getCommitActivity (Owner o) (RepoName r)
  = get (v3 json) ["repos", o, r, "stats", "code_frequency"] []

getParticipation
  :: GitHubM s m
  => Owner
  -> RepoName
  -> m (GitHubResponse Participation)
getWeeklyCommitCounts (Owner o) (RepoName r)
  = get (v3 json) ["repos", o, r, "stats", "participation"] []

getPunchCard
  :: GitHubM s m
  => Owner
  -> RepoName
  -> m (GitHubResponse (Vector PunchCard))
getPunchCard (Owner o) (RepoName r)
  = get (v3 json) ["repos", o, r, "stats", "punch_card"] []


