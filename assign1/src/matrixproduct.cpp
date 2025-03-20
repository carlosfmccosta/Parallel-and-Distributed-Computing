#include <stdio.h>
#include <iostream>
#include <iomanip>
#include <time.h>
#include <cstdlib>
#include <papi.h>
#include <fstream>

using namespace std;

#define SYSTEMTIME clock_t

void OnMult(int m_ar, int m_br)
{

	SYSTEMTIME Time1, Time2;

	char st[100];
	double temp;
	int i, j, k;

	double *pha, *phb, *phc;

	pha = (double *)malloc((m_ar * m_ar) * sizeof(double));
	phb = (double *)malloc((m_ar * m_ar) * sizeof(double));
	phc = (double *)malloc((m_ar * m_ar) * sizeof(double));

	for (i = 0; i < m_ar; i++)
		for (j = 0; j < m_ar; j++)
			pha[i * m_ar + j] = (double)1.0;

	for (i = 0; i < m_br; i++)
		for (j = 0; j < m_br; j++)
			phb[i * m_br + j] = (double)(i + 1);

	Time1 = clock();

	for (i = 0; i < m_ar; i++)
	{
		for (j = 0; j < m_br; j++)
		{
			temp = 0;
			for (k = 0; k < m_ar; k++)
			{
				temp += pha[i * m_ar + k] * phb[k * m_br + j];
			}
			phc[i * m_ar + j] = temp;
		}
	}

	Time2 = clock();
	sprintf(st, "Time: %3.9f seconds\n", (double)(Time2 - Time1) / CLOCKS_PER_SEC);
	cout << st;

	// display 10 elements of the result matrix tto verify correctness
	cout << "Result matrix: " << endl;
	for (i = 0; i < 1; i++)
	{
		for (j = 0; j < min(10, m_br); j++)
			cout << phc[j] << " ";
	}
	cout << endl;

	free(pha);
	free(phb);
	free(phc);
}

// add code here for line x line matriz multiplication
void OnMultLine(int m_ar, int m_br)
{
	SYSTEMTIME Time1, Time2;

	char st[100];
	double temp;
	int i, j, k;

	double *pha, *phb, *phc;

	pha = (double *)malloc((m_ar * m_ar) * sizeof(double));
	phb = (double *)malloc((m_ar * m_ar) * sizeof(double));
	phc = (double *)malloc((m_ar * m_ar) * sizeof(double));

	for (i = 0; i < m_ar; i++)
		for (j = 0; j < m_ar; j++)
			pha[i * m_ar + j] = (double)1.0;

	for (i = 0; i < m_br; i++)
		for (j = 0; j < m_br; j++)
			phb[i * m_br + j] = (double)(i + 1);

	
	for (i = 0; i < m_ar; i++) {         
		for (j = 0; j < m_br; j++) {
			phc[i * m_br + j] = 0.0;  // Initialize phc
		}
	}
		
	Time1 = clock();

	for (int i = 0; i < m_ar; i++) {         
        for (int k = 0; k < m_ar; k++) {     
            double A_ik = pha[i * m_ar + k];
            for (int j = 0; j < m_br; j++) { 
                phc[i * m_br + j] += A_ik * phb[k * m_br + j];
            }
        }
    }


	Time2 = clock();
	sprintf(st, "Time: %3.9f seconds\n", (double)(Time2 - Time1) / CLOCKS_PER_SEC);
	cout << st;

	// display 10 elements of the result matrix tto verify correctness
	cout << "Result matrix: " << endl;
	for (i = 0; i < 1; i++)
	{
		for (j = 0; j < min(10, m_br); j++)
			cout << phc[j] << " ";
	}
	cout << endl;

	free(pha);
	free(phb);
	free(phc);
}

// add code here for block x block matriz multiplication
void OnMultBlock(int m_ar, int m_br, int bkSize)
{
	SYSTEMTIME Time1, Time2;

	char st[100];
	double temp;
	int i, j, k;

	double *pha, *phb, *phc;

	pha = (double *)malloc((m_ar * m_ar) * sizeof(double));
	phb = (double *)malloc((m_ar * m_ar) * sizeof(double));
	phc = (double *)malloc((m_ar * m_ar) * sizeof(double));

	for (i = 0; i < m_ar; i++)
		for (j = 0; j < m_ar; j++)
			pha[i * m_ar + j] = (double)1.0;

	for (i = 0; i < m_br; i++)
		for (j = 0; j < m_br; j++)
			phb[i * m_br + j] = (double)(i + 1);

	// Initialize result matrix
    for (int i = 0; i < m_ar; i++)
        for (int j = 0; j < m_br; j++)
            phc[i * m_br + j] = 0.0;

	Time1 = clock();

	
	for (int bi = 0; bi < m_ar; bi += bkSize) {         //block rows
		for (int bj = 0; bj < m_br; bj += bkSize) {     //block columns
			for (int bk = 0; bk < m_ar; bk += bkSize) { //block depth

				//multiply sub-matrix (block)
				for (int i = bi; i < min(bi + bkSize, m_ar); i++) {
					for (int j = bj; j < min(bj + bkSize, m_br); j++) {
						double sum = 0.0;
						for (int k = bk; k < min(bk + bkSize, m_ar); k++) {
							sum += pha[i * m_ar + k] * phb[k * m_br + j];
						}
						phc[i * m_br + j] += sum;
					}
				}
			}
		}
	}

	Time2 = clock();
	sprintf(st, "Time: %3.9f seconds\n", (double)(Time2 - Time1) / CLOCKS_PER_SEC);
	cout << st;

	// display 10 elements of the result matrix tto verify correctness
	cout << "Result matrix: " << endl;
	for (i = 0; i < 1; i++)
	{
		for (j = 0; j < min(10, m_br); j++)
			cout << phc[j] << " ";
	}
	cout << endl;

	free(pha);
	free(phb);
	free(phc);
}

void OnMultLineParallel(int m_ar, int m_br)
{
    SYSTEMTIME Time1, Time2;

    double *pha, *phb, *phc;

    pha = (double *)malloc((m_ar * m_ar) * sizeof(double));
    phb = (double *)malloc((m_ar * m_ar) * sizeof(double));
    phc = (double *)malloc((m_ar * m_ar) * sizeof(double));

    for (int i = 0; i < m_ar; i++)
        for (int j = 0; j < m_ar; j++)
            pha[i * m_ar + j] = 1.0;

    for (int i = 0; i < m_br; i++)
        for (int j = 0; j < m_br; j++)
            phb[i * m_br + j] = (double)(i + 1);

    for (int i = 0; i < m_ar; i++)        
        for (int j = 0; j < m_br; j++)
            phc[i * m_br + j] = 0.0;

    Time1 = clock();

    #pragma omp parallel for
    for (int i = 0; i < m_ar; i++) {    
        for (int k = 0; k < m_ar; k++) {     
            double A_ik = pha[i * m_ar + k];
            for (int j = 0; j < m_br; j++) { 
                phc[i * m_br + j] += A_ik * phb[k * m_br + j];
            }
        }
    }

	

    Time2 = clock();
    double elapsedTime = (double)(Time2 - Time1) / CLOCKS_PER_SEC;

    outFile << "OnMultLineParallel, " << m_ar << ", " << elapsedTime << endl;

    free(pha);
    free(phb);
    free(phc);
}

void OnMultLineParallel2(int m_ar, int m_br)
{
    SYSTEMTIME Time1, Time2;
    char st[100];
    double *pha, *phb, *phc;

    pha = (double *)malloc((m_ar * m_ar) * sizeof(double));
    phb = (double *)malloc((m_ar * m_ar) * sizeof(double));
    phc = (double *)malloc((m_ar * m_ar) * sizeof(double));

    for (int i = 0; i < m_ar; i++)
        for (int j = 0; j < m_ar; j++)
            pha[i * m_ar + j] = (double)1.0;

    for (int i = 0; i < m_br; i++)
        for (int j = 0; j < m_br; j++)
            phb[i * m_br + j] = (double)(i + 1);

    for (int i = 0; i < m_ar; i++)
        for (int j = 0; j < m_br; j++)
            phc[i * m_br + j] = 0.0;

    Time1 = clock();

    #pragma omp parallel for collapse(2)
    for (int i = 0; i < m_ar; i++) {         
        for (int k = 0; k < m_ar; k++) {     
            double A_ik = pha[i * m_ar + k];

            #pragma omp parallel for reduction(+:phc[i * m_br : m_br])
            for (int j = 0; j < m_br; j++) { 
                phc[i * m_br + j] += A_ik * phb[k * m_br + j];
            }
        }
    }

    Time2 = clock();
    double elapsedTime = (double)(Time2 - Time1) / CLOCKS_PER_SEC;

    outFile << "OnMultLineParallel2, " << m_ar << ", " << elapsedTime << endl;


    free(pha);
    free(phb);
    free(phc);
}


void handle_error(int retval)
{
	printf("PAPI error %d: %s\n", retval, PAPI_strerror(retval));
	exit(1);
}

void init_papi()
{
	int retval = PAPI_library_init(PAPI_VER_CURRENT);
	if (retval != PAPI_VER_CURRENT && retval < 0)
	{
		printf("PAPI library version mismatch!\n");
		exit(1);
	}
	if (retval < 0)
		handle_error(retval);

	std::cout << "PAPI Version Number: MAJOR: " << PAPI_VERSION_MAJOR(retval)
			  << " MINOR: " << PAPI_VERSION_MINOR(retval)
			  << " REVISION: " << PAPI_VERSION_REVISION(retval) << "\n";
}

int main()
{
    ofstream outFile("results.csv");
    if (!outFile.is_open()) {
        cerr << "Error opening file!" << endl;
        return 1;
    }

    // Write header to file
    outFile << "Function, Matrix Size, Time (seconds)" << endl;

    // Matrix sizes to test
    int sizes[] = {600, 600, 600, 600, 600,
                   1000, 1000, 1000, 1000, 1000,
                   1400, 1400, 1400, 1400, 1400,
                   1800, 1800, 1800, 1800, 1800,
                   2200, 2200, 2200, 2200, 2200,
                   2600, 2600, 2600, 2600, 2600,
                   3000, 3000, 3000, 3000, 3000,
                   4096, 4096, 4096, 4096, 4096,
                   6144, 6144, 6144, 6144, 6144,
                   8192, 8192, 8192, 8192, 8192,
                   10240, 10240, 10240, 10240, 10240};

    // Run OnMultLineParallel for all sizes
    for (int size : sizes) {
        OnMultLineParallel(size, size, outFile);
    }

    // Run OnMultLineParallel2 for all sizes
    for (int size : sizes) {
        OnMultLineParallel2(size, size, outFile);
    }

    outFile.close();
    cout << "Results written to results.csv" << endl;

    return 0;
}
