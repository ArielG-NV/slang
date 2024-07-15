#ifndef SLANG_PRELUDE_CPP_CUDA_SHARED_H
#define SLANG_PRELUDE_CPP_CUDA_SHARED_H

template<typename T>
struct _slang_mul_helper
{
    static SLANG_FORCE_INLINE SLANG_CUDA_CALL T add(T left, T right);
    static SLANG_FORCE_INLINE SLANG_CUDA_CALL T mul(T left, T right);
};

template <>
bool _slang_mul_helper<bool>::add(bool left, bool right)
{
    return bool(left | right);
}

template <>
bool _slang_mul_helper<bool>::mul(bool left, bool right)
{
    return bool(left & right);
}

template <typename T>
T _slang_mul_helper<T>::add(T left, T right)
{
    return left + right;
}

template <typename T>
T _slang_mul_helper<T>::mul(T left, T right)
{
    return left * right;
}

template<typename T, int N>
SLANG_FORCE_INLINE SLANG_CUDA_CALL T _slang_dot(Vector<T, N> x, Vector<T, N> y)
{
    T result = T(0);
    for(int i = 0; i < N; i++)
        result += _slang_vector_get_element(x, i) * _slang_vector_get_element(y, i);
    return result;
}

template<typename T, int N>
SLANG_FORCE_INLINE SLANG_CUDA_CALL T _slang_mul(Vector<T, N> x, Vector<T, N> y)
{
    return _slang_dot(x, y);
}

template<typename T, int N, int M>
SLANG_FORCE_INLINE SLANG_CUDA_CALL Vector<T,M> _slang_mul(Vector<T, N> left, Matrix<T, N, M> right)
{
    Vector<T,M> result;
    for(int j = 0; j < M; ++j)
    {
        T sum = T(0);
        for(int i = 0; i < N; ++i)
        {
            sum = _slang_mul_helper<T>::add(sum, _slang_mul_helper<T>::mul(
                _slang_vector_get_element(left, i), _slang_vector_get_element(right[i], j)
                ));
        }
        *_slang_vector_get_element_ptr(&result, j) = sum;
    }
    return result;
}

template<typename T, int N, int M>
SLANG_FORCE_INLINE SLANG_CUDA_CALL Vector<T,N> _slang_mul(Matrix<T, N, M> left, Vector<T, M> right)
{
    Vector<T,N> result;
    for(int i = 0; i < N; ++i)
    {
        T sum = T(0);
        for(int j = 0; j < M; ++j)
        {
            sum = _slang_mul_helper<T>::add(sum, _slang_mul_helper<T>::mul(
                _slang_vector_get_element(left[i], j), _slang_vector_get_element(right, j)
                ));
        }
        *_slang_vector_get_element_ptr(&result, i) = sum;
    }
    return result;
}

template<typename T, int R, int N, int C>
SLANG_FORCE_INLINE SLANG_CUDA_CALL Matrix<T,R,C> _slang_mul(Matrix<T, R, N> left, Matrix<T, N, C> right)
{
    Matrix<T,R,C> result;
    for(int r = 0; r < R; ++r)
    for(int c = 0; c < C; ++c)
    {
        T sum = T(0);
        for(int i = 0; i < N; ++i)
        {
            sum = _slang_mul_helper<T>::add(sum, _slang_mul_helper<T>::mul(
                    _slang_vector_get_element(left[r], i), _slang_vector_get_element(right[i],c)
                ));
        }
        *_slang_vector_get_element_ptr(&result[r], c) = sum;
    }
    return result;
}

template<typename T, int N, int M>
SLANG_FORCE_INLINE SLANG_CUDA_CALL Matrix<T,M,N> _slang_transpose(Matrix<T, N, M> x)
{
    Matrix<T, M, N> result;
    for (int r = 0; r < M; ++r)
        for (int c = 0; c < N; ++c)
            *_slang_vector_get_element_ptr(&result[r], c) = _slang_vector_get_element(x[c], r);
    return result;
}

#endif
