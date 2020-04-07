pipeline {
  agent any
  stages {
    stage('Build') {
      agent {
        docker {
          image 'erlang/latest'
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