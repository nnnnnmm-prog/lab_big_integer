#include "big_integer.h"
#include <algorithm>
#include <string>
#include <iostream>
#include <vector>

using namespace std;



void clean(vector<int>& a) {
    while (a.size() > 1 && a.back() == 0) {
        a.pop_back();
    }
}

int cmp(const vector<int>& a, const vector<int>& b) {
    if (a.size() != b.size()) {
        return a.size() < b.size() ? -1 : 1;
    }
    for (int i = (int)a.size() - 1; i >= 0; i--) {
        if (a[i] != b[i]) {
            return a[i] < b[i] ? -1 : 1;
        }
    }
    return 0;
}


BigInteger::BigInteger() {
    digits_ = {0};
    negative_ = false;
}

BigInteger::BigInteger(int x) {
    long long n = x;
    negative_ = (n < 0);
    if (n < 0) n = -n;

    digits_.clear();
    if (n == 0) {
        digits_ = {0};
    } else {
        while (n > 0) {
            digits_.push_back(n % 10);
            n /= 10;
        }
    }

    clean(digits_);
    if (digits_.size() == 1 && digits_[0] == 0) negative_ = false;
}

BigInteger::BigInteger(long long n) {
    negative_ = (n < 0);
    if (n < 0) n = -n;

    digits_.clear();
    if (n == 0) {
        digits_ = {0};
    } else {
        while (n > 0) {
            digits_.push_back(n % 10);
            n /= 10;
        }
    }

    clean(digits_);
    if (digits_.size() == 1 && digits_[0] == 0) negative_ = false;
}

BigInteger::BigInteger(const string& s) {
    digits_.clear();
    negative_ = false;

    int i = 0;
    if (s[0] == '-') {
        negative_ = true;
        i = 1;
    } else if (s[0] == '+') {
        i = 1;
    }

    for (int j = (int)s.size() - 1; j >= i; j--) {
        digits_.push_back(s[j] - '0');
    }

    if (digits_.empty()) digits_ = {0};
    clean(digits_);
    if (digits_.size() == 1 && digits_[0] == 0) negative_ = false;
}

// ================= СРАВНЕНИЕ =================

bool BigInteger::operator==(const BigInteger& b) const {
    return negative_ == b.negative_ && digits_ == b.digits_;
}

bool BigInteger::operator!=(const BigInteger& b) const {
    return !(*this == b);
}

bool BigInteger::operator<(const BigInteger& b) const {
    if (negative_ != b.negative_) {
        return negative_;
    }

    int c = cmp(digits_, b.digits_);

    if (!negative_) return c < 0;
    else return c > 0;
}

bool BigInteger::operator<=(const BigInteger& b) const {
    return *this < b || *this == b;
}

bool BigInteger::operator>(const BigInteger& b) const {
    return !(*this <= b);
}

bool BigInteger::operator>=(const BigInteger& b) const {
    return !(*this < b);
}



BigInteger& BigInteger::operator+=(const BigInteger& b) {
    if (negative_ == b.negative_) {
        int carry = 0;
        size_t max_len = max(digits_.size(), b.digits_.size());

        for (size_t i = 0; i < max_len || carry; i++) {
            int sum = carry;
            if (i < digits_.size()) sum += digits_[i];
            if (i < b.digits_.size()) sum += b.digits_[i];

            if (i < digits_.size()) digits_[i] = sum % 10;
            else digits_.push_back(sum % 10);

            carry = sum / 10;
        }
        return *this;
    }

    int c = cmp(digits_, b.digits_);

    if (c == 0) {
        digits_ = {0};
        negative_ = false;
        return *this;
    }

    if (c < 0) {
        digits_ = b.digits_;
        negative_ = b.negative_;
    }

    int borrow = 0;
    for (size_t i = 0; i < digits_.size(); i++) {
        int diff = digits_[i] - borrow;
        if (i < b.digits_.size()) diff -= b.digits_[i];

        if (diff < 0) {
            diff += 10;
            borrow = 1;
        } else {
            borrow = 0;
        }

        digits_[i] = diff;
    }

    clean(digits_);
    if (digits_.size() == 1 && digits_[0] == 0) negative_ = false;

    return *this;
}

BigInteger BigInteger::operator+(const BigInteger& b) const {
    BigInteger r = *this;
    r += b;
    return r;
}


BigInteger& BigInteger::operator-=(const BigInteger& b) {
    BigInteger t = b;
    if (!t.is_zero()) {
        t.negative_ = !t.negative_;
    }
    *this += t;
    return *this;
}

BigInteger BigInteger::operator-(const BigInteger& b) const {
    BigInteger r = *this;
    r -= b;
    return r;
}


BigInteger BigInteger::operator*(const BigInteger& b) const {
    if (is_zero() || b.is_zero()) return BigInteger(0);

    BigInteger r;
    r.digits_.assign(digits_.size() + b.digits_.size(), 0);

    for (size_t i = 0; i < digits_.size(); i++) {
        int carry = 0;
        for (size_t j = 0; j < b.digits_.size() || carry; j++) {
            long long cur = r.digits_[i + j] + carry;
            if (j < b.digits_.size()) {
                cur += 1LL * digits_[i] * b.digits_[j];
            }
            r.digits_[i + j] = cur % 10;
            carry = cur / 10;
        }
    }

    clean(r.digits_);
    r.negative_ = (negative_ != b.negative_);
    if (r.is_zero()) r.negative_ = false;

    return r;
}

BigInteger& BigInteger::operator*=(const BigInteger& b) {
    *this = *this * b;
    return *this;
}


BigInteger BigInteger::operator/(const BigInteger& b) const {
    if (b.is_zero()) {
        throw runtime_error("Division by zero");
    }

    if (cmp(digits_, b.digits_) < 0) {
        return BigInteger(0);
    }

    bool sign_res = (negative_ != b.negative_);

    BigInteger a = *this;
    BigInteger d = b;
    a.negative_ = false;
    d.negative_ = false;

    vector<int> res;
    BigInteger cur;

    for (int i = (int)a.digits_.size() - 1; i >= 0; i--) {
        cur.digits_.insert(cur.digits_.begin(), a.digits_[i]);
        clean(cur.digits_);

        int cnt = 0;
        while (cmp(cur.digits_, d.digits_) >= 0) {
            int borrow = 0;
            for (size_t j = 0; j < cur.digits_.size(); j++) {
                int diff = cur.digits_[j] - borrow;
                if (j < d.digits_.size()) diff -= d.digits_[j];

                if (diff < 0) {
                    diff += 10;
                    borrow = 1;
                } else {
                    borrow = 0;
                }
                cur.digits_[j] = diff;
            }
            clean(cur.digits_);
            cnt++;
        }
        res.insert(res.begin(), cnt);
    }

    clean(res);

    BigInteger r;
    r.digits_ = res;
    r.negative_ = sign_res;
    if (r.is_zero()) r.negative_ = false;

    return r;
}

BigInteger BigInteger::operator%(const BigInteger& b) const {
    return *this - (*this / b) * b;
}

BigInteger& BigInteger::operator/=(const BigInteger& b) {
    *this = *this / b;
    return *this;
}

BigInteger& BigInteger::operator%=(const BigInteger& b) {
    *this = *this % b;
    return *this;
}


BigInteger BigInteger::operator-() const {
    BigInteger r = *this;
    if (!r.is_zero()) {
        r.negative_ = !r.negative_;
    }
    return r;
}

BigInteger& BigInteger::operator++() {
    *this += 1;
    return *this;
}

BigInteger BigInteger::operator++(int) {
    BigInteger old = *this;
    ++(*this);
    return old;
}

BigInteger& BigInteger::operator--() {
    *this -= 1;
    return *this;
}

BigInteger BigInteger::operator--(int) {
    BigInteger old = *this;
    --(*this);
    return old;
}


string BigInteger::to_string() const {  
    if (is_zero()) return "0";

    string s;              


    
    if (negative_) s = "-";

    for (int i = (int)digits_.size() - 1; i >= 0; i--) {
        s += char('0' + digits_[i]);
    }

    return s;
}


bool BigInteger::is_zero() const {
    return digits_.size() == 1 && digits_[0] == 0;
}

bool BigInteger::is_negative() const {
    return negative_;
}

BigInteger::operator bool() const {
    return !is_zero();
}


ostream& operator<<(ostream& os, const BigInteger& x) {
    os << x.to_string();
    return os;
}

istream& operator>>(istream& is, BigInteger& x) {
    string s;
    is >> s;
    x = BigInteger(s);
    return is;
}