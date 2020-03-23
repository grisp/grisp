pipeline {
  agent any
  stages {
    stage('Build') {
      steps {
        sh 'rebar3 compile'
      }
    }

    stage('Test') {
      steps {
        sh 'rebar3 eunit'
      }
    }

  }
}