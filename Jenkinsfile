pipeline {
    agent any
    options {
        copyArtifactPermission('/git/inTextSummaryTable/*,/git/patientProfilesVis/*,/git/clinDataReview/*');
        buildDiscarder(logRotator(numToKeepStr: '3'))
    }
    environment {
        IMAGE = 'clinutils'
        NS = 'shared'
        REGISTRY = 'registry.openanalytics.eu'
        TAG = sh(returnStdout: true, script: "echo $BRANCH_NAME | sed -e 's/[A-Z]/\\L&/g' -e 's/[^a-z0-9._-]/./g'").trim()
        REGION = 'eu-west-1'
        NOT_CRAN = 'true'
    }
    stages {
        stage('Build Image') {
            agent {
                kubernetes {
                    yaml '''
                    apiVersion: v1
                    kind: Pod
                    spec:
                      imagePullSecrets:
                        - name: registry-robot
                      volumes:
                        - name: kaniko-dockerconfig
                          secret:
                            secretName: registry-robot
                      containers:
                      - name: kaniko
                        image: gcr.io/kaniko-project/executor:v1.9.1-debug
                        env:
                        - name: AWS_SDK_LOAD_CONFIG
                          value: "true"
                        command:
                        - /kaniko/docker-credential-ecr-login
                        - get
                        tty: true
                        resources:
                          requests:
                              memory: "2Gi"
                          limits:
                              memory: "6Gi"
                              ephemeral-storage: "10Gi"
                        imagePullPolicy: Always
                        volumeMounts:
                          - name: kaniko-dockerconfig
                            mountPath: /kaniko/.docker/config.json
                            subPath: .dockerconfigjson
                    '''
                    defaultContainer 'kaniko'
                }
            }
            steps {
                container('kaniko') {
                    sh """/kaniko/executor \
                    	-v info \
                    	--context ${env.WORKSPACE} \
                    	--cache=true \
                    	--cache-ttl=8760h0m0s \
                    	--cache-repo ${env.REGISTRY}/${env.NS}/${env.IMAGE} \
                    	--cleanup \
                    	--destination ${env.REGISTRY}/${env.NS}/${env.IMAGE}:${env.TAG} \
                    	--registry-mirror ${env.REGISTRY}"""
                }
            }
        }
        stage('Packages') {
            agent {
                kubernetes {
                    yaml """
                    apiVersion: v1
                    kind: Pod
                    spec:
                      imagePullSecrets:
                        - name: registry-robot
                      containers:
                        - name: r
                          image: ${env.REGISTRY}/${env.NS}/${env.IMAGE}:${env.TAG}
                          command: 
                            - cat
                          tty: true
                          imagePullPolicy: Always"""
                    defaultContainer 'r'
                }
            }
            stages {
                stage('clinUtils') {
                    stages {
                        stage('Roxygen') {
                            steps {
                                sh 'R -q -e \'roxygen2::roxygenize("clinUtils")\''
                            }
                        }
                        stage('Build') {
                            steps {
                                sh 'R CMD build clinUtils'
                            }
                        }
                        stage('Check') {
                            steps {
                                script() {
                                    switch(sh(script: 'ls clinUtils_*.tar.gz && R CMD check clinUtils_*.tar.gz', returnStatus: true)) {
                                        case 0: currentBuild.result = 'SUCCESS'
                                        default: currentBuild.result = 'FAILURE'; error('script exited with failure status')
                                    }
                                }
                            }
                        }
                        stage('Install') {
                            steps {
                                sh 'R -q -e \'install.packages(list.files(".", "clinUtils_.*.tar.gz"), repos = NULL)\''
                            }
                        }
                        stage('Test and coverage') {
                            steps {
                                dir('.') {
                                    sh '''R -q -e \'code <- "testthat::test_package(\\"clinUtils\\", reporter = testthat::MultiReporter$new(list(testthat::ProgressReporter$new(file = file.path(getwd(), \\"results.txt\\")), testthat::JunitReporter$new(file = file.path(getwd(), \\"results.xml\\")))))"
                                    packageCoverage <- covr::package_coverage("clinUtils", type = "none", code = code)
                                    cat(paste(readLines(file.path(getwd(), "results.txt")), collapse="\n"), "\n")
                                    covr::report(x = packageCoverage, file = paste0("testCoverage-", attr(packageCoverage, "package")$package, "-", attr(packageCoverage, "package")$version, ".html"));
                                    covr::to_cobertura(packageCoverage)\''''
                                    sh 'zip -r testCoverage.zip lib/ testCoverage*.html'
                                }
                            }
                            post {
                                always {
                                    dir('.') {
                                        junit 'results.xml'
                                        cobertura autoUpdateHealth: false, autoUpdateStability: false, coberturaReportFile: 'cobertura.xml', conditionalCoverageTargets: '70, 0, 0', failUnhealthy: false, failUnstable: false, lineCoverageTargets: '80, 0, 0', maxNumberOfBuilds: 0, methodCoverageTargets: '80, 0, 0', onlyStable: false, sourceEncoding: 'ASCII', zoomCoverageChart: false
                                    }
                                }
                            }
                        }
                    }
                }
                stage('Archive artifacts') {
                    steps {
                        archiveArtifacts artifacts: '*.tar.gz, *.pdf, **/00check.log, test-results.txt, testCoverage.zip', fingerprint: true
                    }
                }
            }
        }
    }
}

