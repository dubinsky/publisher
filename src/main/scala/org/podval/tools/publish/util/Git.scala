package org.podval.tools.publish.util

import org.eclipse.jgit.api.Git
import org.eclipse.jgit.api.errors.GitAPIException
import org.eclipse.jgit.diff.DiffConfig
import org.eclipse.jgit.lib.{Constants, Repository}
import org.eclipse.jgit.revwalk.{FollowFilter, RevCommit, RevWalk}
import org.eclipse.jgit.storage.file.FileRepositoryBuilder

import java.io.{File, IOException}
import java.util.Date

final class Git(rootDirectory: File):
  private lazy val repository: Option[Repository] =
    val dotGit = File(rootDirectory, ".git")
    Option.when(dotGit.exists)(FileRepositoryBuilder().setGitDir(dotGit).build)

//  def modificationDate(filePath: String) = repository.map: repository =>
//    val revWalk: RevWalk = RevWalk(repository) // TODO release
//    revWalk.markStart(revWalk.parseCommit(repository.resolve(Constants.HEAD)))
//        revWalk.setRevFilter(FollowFilter.create(filePath, DiffConfig.KEY))
//        val lastCommit: RevCommit = revWalk.iterator.next
//        // Get commit time (seconds since epoch)
//        val commitTime: Int = lastCommit.getCommitTime


//    try (Git git = Git.open(repoDir)) {
//      Iterable < RevCommit > commits = git.log()
//        .addPath(filePath)
//        .setMaxCount(1) // Only fetch the most recent commit
//        .call();
//
//      for (RevCommit commit: commits) {
//        // Returns the Unix timestamp (seconds since epoch)
//        int commitTimeSeconds = commit.getCommitTime();
//        return new Date((long) commitTimeSeconds *
//        1000
//        );
//      }
//    }
//    return null; // File has no commit history in the repo
//  }
//}