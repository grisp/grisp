pipeline {
  agent any
  stages {
    stage('Build') {
      steps {
        sh 'rebar3 compile'
      }
    }

    stage('') {
      steps {
        sh 'rebar3 eunit'
      }
    }

  }
}