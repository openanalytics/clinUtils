pipeline {
    agent any
    options {
        buildDiscarder(logRotator(numToKeepStr: '3'))
    }
    environment {
        IMAGE = 'clinutils'
        NS = 'oa'
        REG = '196229073436.dkr.ecr.eu-west-1.amazonaws.com'
        TAG = sh(returnStdout: true, script: "echo $BRANCH_NAME | sed -e 's/[A-Z]/\\L&/g' -e 's/[^a-z0-9._-]/./g'").trim()
        DOCKER_BUILDKIT = '1'
    }
    stages {
        stage('Build Image') {
            agent {
                kubernetes {
                    yaml '''
                    apiVersion: v1
                    kind: Pod
                    spec:
                      containers:
                      - name: dind
                        image: 196229073436.dkr.ecr.eu-west-1.amazonaws.com/oa-infrastructure/dind
                        securityContext:
                          privileged: true
                        resources:
                            requests: 
                                memory: "1024Mi"
                            limits:
                                memory: "1536Mi"
                    '''
                    defaultContainer 'dind'
                }
            }
            steps {
                ecrPull "${env.REG}", "${env.NS}/${env.IMAGE}", "${env.TAG}", '', 'eu-west-1'
                sh "docker build --cache-from ${env.REG}/${env.NS}/${env.IMAGE}:${env.TAG} -t ${env.NS}/${env.IMAGE}:${env.TAG} -f Dockerfile.build ."
                ecrPush "${env.REG}", "${env.NS}/${env.IMAGE}", "${env.TAG}", '', 'eu-west-1'
            }
        }
        stage('Packages') {
            agent {
                kubernetes {
                    yaml """
                    apiVersion: v1
                    kind: Pod
                    spec:
                      containers:
                      - name: r
                        image: ${env.REG}/${env.NS}/${env.IMAGE}:${env.TAG}
                        command: 
                        - cat
                        tty: true
                        imagePullPolicy: Always
                        resources:
                            requests: 
                                memory: "1024Mi"
                            limits:
                                memory: "1536Mi"
                     """
                    defaultContainer 'r'
                }
            }
            stages {
                stage('clinUtils') {
                    stages {
                        stage('Roxygen') {
                            steps {
                                sh 'R -q -e \'roxygen2::roxygenize("package/clinUtils")\''
                            }
                        }
                        stage('Build') {
                            steps {
                                sh 'R CMD build package/clinUtils'
                            }
                        }
                        stage('Check') {
                            steps {
                                sh '''
                                export TESTTHAT_DEFAULT_CHECK_REPORTER="junit"
                                export TESTTHAT_OUTPUT_FILE="results.xml"
                                ls clinUtils_*.tar.gz && R CMD check clinUtils_*.tar.gz --no-manual
                                zip -r tests.zip *.Rcheck/tests > /dev/null 2>&1
                                '''
                            }
                            post {
                                always {
                                    junit "*.Rcheck/tests/results.xml"
                                }
                                failure {
                                    sh 'zip -r tests.zip *.Rcheck/tests > /dev/null 2>&1'
                                }
                            }
                        }
                        stage('Coverage') {
                           steps {
                                sh '''
                                R -q -e \'
                                pc <- covr::package_coverage("package/clinUtils", type = "none", code = "testthat::test_package(\\"clinUtils\\", reporter = testthat::JunitReporter$new(file = file.path(Sys.getenv(\\"WORKSPACE\\"), \\"results.xml\\")))");
                                covr::report(x = pc, file = paste0("testCoverage-", attr(pc, "package")$package, "-", attr(pc, "package")$version, ".html"))
                                covr::to_cobertura(pc)
                                \'
                                zip -r testCoverage.zip lib/ testCoverage*.html
                                '''
                            }
                           post {
                              always {
                                  cobertura autoUpdateHealth: false, autoUpdateStability: false, coberturaReportFile: 'cobertura.xml', conditionalCoverageTargets: '70, 0, 0', failUnhealthy: false, failUnstable: false, lineCoverageTargets: '80, 0, 0', maxNumberOfBuilds: 0, methodCoverageTargets: '80, 0, 0', onlyStable: false, sourceEncoding: 'ASCII', zoomCoverageChart: false
                              }
                           }
                        }
                        stage('Install') {
                            steps {
                                sh 'R -q -e \'install.packages(list.files(".", "clinUtils_.*.tar.gz"), repos = NULL) \''
                            }
                        }
                    }
                }
                stage('Archive artifacts') {
                    steps {
                        archiveArtifacts artifacts: '*.tar.gz, *.pdf, **/00check.log, **/testthat.Rout, testCoverage.zip', fingerprint: true
                    }
                }
            }
        }
    }
}

