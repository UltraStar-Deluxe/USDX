#include <opencv2/core.hpp>
#include <opencv2/imgproc.hpp>
#include <opencv2/videoio.hpp>
#include <cstdio>

// OpenCV 4 no longer defines these by default
#define CV_CANNY_L2_GRADIENT	(1 << 31)
#define CV_BLUR			1
#define CV_CHOLESKY		3
#define CV_SVD			1
#define CV_SVD_SYM		2

// The idea is to use the UMat for all calculations while keeping the Mat empty.
// Only when the Pascal code requests a pointer to the image data, we assign
// getMat to the Mat to keep a reference to that buffer.
struct UMatWrapper {
	cv::UMat umat;
	cv::Mat mat;
};

struct USDX_CvCapture {
	USDX_CvCapture(int index) : cap(index) {}

	cv::VideoCapture cap;
	UMatWrapper frame;
};

extern "C" {

int USDX_cvGetDimSize(UMatWrapper *w, int index);
int Get_UMat_depth(UMatWrapper *);
const void *Get_UMat_as_8UC3(UMatWrapper *w);
UMatWrapper *USDX_cvCreateImage(int width, int height, int depth, int channels);
UMatWrapper *USDX_cvCloneImage(UMatWrapper *src);
void USDX_cvReleaseImage(UMatWrapper **w);
void USDX_cvNot(UMatWrapper *src, UMatWrapper *dst);
void USDX_cvAbsDiff(UMatWrapper *src1, UMatWrapper *src2, UMatWrapper *dst);
void USDX_cvCvtColor(UMatWrapper *src, UMatWrapper *dst, int code);
void USDX_cvEqualizeHist(UMatWrapper *src, UMatWrapper *dst);
void USDX_cvCanny(UMatWrapper *src, UMatWrapper *dst, double threshold1, double threshold2, int aperture_size);
void USDX_cvFlip(UMatWrapper *src, UMatWrapper *dst, int flip_mode);
double USDX_cvThreshold(UMatWrapper *src, UMatWrapper *dst, double threshold, double max_value, int threshold_type);
void USDX_cvSmooth(UMatWrapper *src, UMatWrapper *dst, int smooth_type, int param1, int param2, double param3, double param4);
void USDX_cvDilate(UMatWrapper *src, UMatWrapper *dst, void *element, int iterations);
void USDX_cvErode(UMatWrapper *src, UMatWrapper *dst, void *element, int iterations);
void USDX_cvTransform(UMatWrapper *src, UMatWrapper *dst, cv::Mat *transmat, cv::Mat *shiftvec);
void USDX_cvGEMM(cv::Mat *A, cv::Mat *B, double alpha, cv::Mat *C, double beta, cv::Mat *D, int flags);
cv::Mat *USDX_cvCreateMat(int rows, int cols, int type);
void USDX_cvReleaseMat(cv::Mat **mat);
void USDX_cvSetZero(cv::Mat *mat);
void USDX_cvSetReal2D(cv::Mat *mat, int y, int x, double value);
double USDX_cvInvert(cv::Mat *src, cv::Mat *dst, int method);
USDX_CvCapture *USDX_cvCreateCameraCapture(int index);
void USDX_cvReleaseCapture(USDX_CvCapture **capture);
UMatWrapper* USDX_cvQueryFrame(USDX_CvCapture *capture);
int USDX_cvSetCaptureProperty(USDX_CvCapture* capture, int property_id, double value);

}

int USDX_cvGetDimSize(UMatWrapper *w, int index)
{
	if (!w)
		return 0;
	switch (index) {
	case 0:
		return w->umat.rows;
	case 1:
		return w->umat.cols;
	default:
		return 1;
	}
}

int Get_UMat_depth(UMatWrapper *w)
{
	if (!w)
		return CV_8U;
	return w->umat.depth();
}

const void *Get_UMat_as_8UC3(UMatWrapper *w)
{
	if (!w)
		return NULL;
	try {
		if (w->umat.type() == CV_8UC3) {
			w->mat = w->umat.getMat(cv::ACCESS_READ);
		} else {
			cv::UMat tmp, tmp2;
			switch (w->umat.depth()) {
			default:
				tmp = w->umat;
				break;
			case CV_8S:
				w->umat.convertTo(tmp, CV_8UC3, 255.0/127);
				break;
			case CV_16U:
				w->umat.convertTo(tmp, CV_8UC3, 255.0/65535);
				break;
			case CV_16S:
				w->umat.convertTo(tmp, CV_8UC3, 255.0/32767);
				break;
			case CV_32S:
				w->umat.convertTo(tmp, CV_8UC3, 255.0/2147483647);
				break;
#if CV_VERSION_MAJOR >= 4
			case CV_16F:
#endif
			case CV_32F:
			case CV_64F:
				w->umat.convertTo(tmp, CV_8UC3, 255.0);
			}
			switch (tmp.channels()) {
			default:
				tmp2 = tmp;
				break;
			case 1:
				cv::cvtColor(tmp, tmp2, cv::COLOR_GRAY2RGB);
			}
			w->mat = tmp2.getMat(cv::ACCESS_READ);
		}
		return w->mat.data;
	} catch (...) {
		return NULL;
	}
}

UMatWrapper *USDX_cvCreateImage(int width, int height, int depth, int channels)
{
	UMatWrapper *w = NULL;
	try {
		cv::UMat umat(height, width, CV_MAKETYPE(depth, channels));
		w = new UMatWrapper;
		w->umat = umat;
	} catch (...) {
		if (w) {
			try {
				delete w;
			} catch (...) {}
			w = NULL;
		}
	}
	return w;
}

UMatWrapper *USDX_cvCloneImage(UMatWrapper *src)
{
	UMatWrapper *w = NULL;
	try {
		w = new UMatWrapper;
		w->umat = src->umat.clone();
	} catch (...) {
		if (w) {
			try {
				delete w;
			} catch(...) {}
			w = NULL;
		}
	}
	return w;
}

void USDX_cvReleaseImage(UMatWrapper **w)
{
	if (!w)
		return;
	try {
		delete *w;
	} catch (...) {}
	*w = NULL;
}

void USDX_cvNot(UMatWrapper *src, UMatWrapper *dst)
{
	if (!src || !dst)
		return;
	try {
		cv::bitwise_not(src->umat, dst->umat);
	} catch (...) {}
}

void USDX_cvAbsDiff(UMatWrapper *src1, UMatWrapper *src2, UMatWrapper *dst)
{
	if (!src2 || !src2 || !dst)
		return;
	try {
		cv::absdiff(src1->umat, src2->umat, dst->umat);
	} catch (...) {}
}

void USDX_cvCvtColor(UMatWrapper *src, UMatWrapper *dst, int code)
{
	if (!src || !dst)
		return;
	try {
		cv::cvtColor(src->umat, dst->umat, code);
	} catch (...) {}
}

void USDX_cvEqualizeHist(UMatWrapper *src, UMatWrapper *dst)
{
	if (!src || !dst)
		return;
	try {
		cv::equalizeHist(src->umat, dst->umat);
	} catch (...) {}
}

void USDX_cvCanny(UMatWrapper *src, UMatWrapper *dst, double threshold1, double threshold2, int aperture_size)
{
	if (!src || !dst)
		return;
	try {
		cv::Canny(src->umat, dst->umat, threshold1, threshold2, aperture_size & 255, (aperture_size & CV_CANNY_L2_GRADIENT) != 0);
	} catch (...) {}
}

void USDX_cvFlip(UMatWrapper *src, UMatWrapper *dst, int flip_mode)
{
	if (!src)
		return;
	try {
		cv::flip(src->umat, dst ? dst->umat : src->umat, flip_mode);
	} catch (...) {}
}

double USDX_cvThreshold(UMatWrapper *src, UMatWrapper *dst, double threshold, double max_value, int threshold_type)
{
	if (src && dst) {
		try {
			cv::UMat dst0(dst->umat);
			double ret = cv::threshold(src->umat, dst->umat, threshold, max_value, threshold_type);
			if (dst0.type() != dst->umat.type()) {
				dst->umat.convertTo(dst0, dst0.type());
				dst->umat = dst0;
			}
			return ret;
		} catch (...) {}
	}
	return 0;
}

void USDX_cvSmooth(UMatWrapper *src, UMatWrapper *dst, int smooth_type, int param1, int param2, double param3, double param4)
{
	if (!src || !dst)
		return;
	if (smooth_type != CV_BLUR) {
		fprintf(stderr, "%s: Only CV_BLUR is implemented", __func__);
		return;
	}
	try {
		cv::boxFilter(src->umat, dst->umat, dst->umat.depth(), cv::Size(param1, param2), cv::Point(-1,-1), true, cv::BORDER_REPLICATE);
	} catch (...) {}
}

void USDX_cvDilate(UMatWrapper *src, UMatWrapper *dst, void *element, int iterations)
{
	if (!src || !dst)
		return;
	if (element) {
		fprintf(stderr, "%s: element != NULL is not implemented", __func__);
		return;
	}
	try {
		cv::dilate(src->umat, dst->umat, cv::Mat(), cv::Point(1,1), iterations, cv::BORDER_REPLICATE);
	} catch (...) {}
}

void USDX_cvErode(UMatWrapper *src, UMatWrapper *dst, void *element, int iterations)
{
	if (!src || !dst)
		return;
	if (element) {
		fprintf(stderr, "%s: element != NULL is not implemented", __func__);
		return;
	}
	try {
		cv::erode(src->umat, dst->umat, cv::Mat(), cv::Point(1,1), iterations, cv::BORDER_REPLICATE);
	} catch (...) {}
}

void USDX_cvTransform(UMatWrapper *src, UMatWrapper *dst, cv::Mat *transmat, cv::Mat *shiftvec)
{
	if (!src || !dst || !transmat)
		return;
	if (shiftvec) {
		fprintf(stderr, "%s: shiftvec != NULL is not implemented", __func__);
		return;
	}

	try {
		cv::transform(src->umat, dst->umat, *transmat);
	} catch (...) {}
}

void USDX_cvGEMM(cv::Mat *A, cv::Mat *B, double alpha, cv::Mat *C, double beta, cv::Mat *D, int flags)
{
	if (!A || !B || !D)
		return;
	if (C) {
		cv::gemm(*A, *B, alpha, *C, beta, *D, flags);
	} else {
		cv::Mat emptyC;
		cv::gemm(*A, *B, alpha, emptyC, beta, *D, flags);
	}
}

cv::Mat *USDX_cvCreateMat(int rows, int cols, int type)
{
	try {
		return new cv::Mat(rows, cols, type);
	} catch(...) {
		return NULL;
	}
}

void USDX_cvReleaseMat(cv::Mat **mat)
{
	if (!mat)
		return;
	try {
		delete *mat;
	} catch(...) {}
	*mat = NULL;
}

void USDX_cvSetZero(cv::Mat *mat)
{
	if (!mat)
		return;
	*mat = cv::Scalar(0);
}

void USDX_cvSetReal2D(cv::Mat *mat, int y, int x, double value)
{
	if (!mat)
		return;
	try {
		switch(mat->depth()) {
		case CV_8U:  mat->at<uchar>(y, x)  = value; return;
		case CV_8S:  mat->at<schar>(y, x)  = value; return;
		case CV_16U: mat->at<ushort>(y, x) = value; return;
		case CV_16S: mat->at<short>(y, x)  = value; return;
#if CV_VERSION_MAJOR >= 4
		case CV_16F: mat->at<cv::float16_t>(y, x) = cv::float16_t(value); return;
#endif
		case CV_32S: mat->at<int>(y, x)    = value; return;
		case CV_32F: mat->at<float>(y, x)  = value; return;
		case CV_64F: mat->at<double>(y, x) = value; return;
		}
	} catch(...) {}
}

double USDX_cvInvert(cv::Mat *src, cv::Mat *dst, int method)
{
	if (!src || !dst)
		return 0;

	cv::DecompTypes d = cv::DECOMP_LU;
	switch (method) {
	case CV_CHOLESKY: d = cv::DECOMP_CHOLESKY; break;
	case CV_SVD:      d = cv::DECOMP_SVD; break;
	case CV_SVD_SYM:  d = cv::DECOMP_EIG; break;
	}

	try {
		return cv::invert(*src, *dst, d);
	} catch(...) {
		return 0;
	}
}

USDX_CvCapture *USDX_cvCreateCameraCapture(int index)
{
	try {
		USDX_CvCapture *c = new USDX_CvCapture(index);
		if (c->cap.isOpened())
			return c;
		delete c;
	} catch(...) {}
	return NULL;

}

void USDX_cvReleaseCapture(USDX_CvCapture **capture)
{
	if (!capture)
		return;
	try {
		delete *capture;
	} catch(...) {}
	*capture = NULL;
}

UMatWrapper* USDX_cvQueryFrame(USDX_CvCapture *capture)
{
	if (capture) {
		try {
			if (capture->cap.read(capture->frame.umat))
				return &capture->frame;
		} catch (...) {}
	}
	return NULL;
}

int USDX_cvSetCaptureProperty(USDX_CvCapture* capture, int property_id, double value)
{
	if (capture) {
		try {
			return capture->cap.set(property_id, value);
		} catch (...) {}
	}
	return 0;
}
