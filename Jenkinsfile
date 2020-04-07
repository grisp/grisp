pipeline {
  agent any
  stages {
    stage('Build') {
      agent {
        docker {
          image 'erlang'
        }

      }
      steps {
        sh '''whoami
rebar3 compile'''
      }
    }

    stage('Test') {
      steps {
        sh 'rebar3 eunit'
      }
    }

  }
}