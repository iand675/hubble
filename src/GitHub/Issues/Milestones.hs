module GitHub.Issues.Milestones where

data MilestoneState = Open | Closed | All

data MilestoneSortBy = DueDate | Completeness

data MilestoneSortDirection = Ascending | Descending

milestones :: Owner
           -> Repo
           -> Maybe MilestoneState
           -> Maybe MilestoneSortBy
           -> Maybe MilestoneSortDirection
           -> m (Page Milestone)

milestone :: Owner
          -> Repo
          -> MilestoneNumber
          -> m Milestone

createMilestone :: Owner
                -> Repo
                -> NewMilestone
                -> m Milestone

updateMilestone :: Owner
                -> Repo
                -> MilestoneNumber
                -> NewMilestone
                -> m Milestone

deleteMilestone :: Owner
                -> Repo
                -> MilestoneNumber
                -> m ()

