// Generated by rstantools.  Do not edit by hand.

/*
    spolr is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    spolr is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with spolr.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#include <rstan/rstaninc.hpp>
// Code generated by Stan version 2.19.1

#include <stan/model/model_header.hpp>

namespace model_spolr_namespace {

using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::prob_grad;
using namespace stan::math;

static int current_statement_begin__;

stan::io::program_reader prog_reader__() {
    stan::io::program_reader reader;
    reader.add_event(0, 0, "start", "model_spolr");
    reader.add_event(24, 22, "end", "model_spolr");
    return reader;
}

#include <stan_meta_header.hpp>
 class model_spolr : public prob_grad {
private:
        int N;
        std::vector<int> Y;
        int K;
        matrix_d X;
        double sd_prior_b;
        int ncat;
public:
    model_spolr(stan::io::var_context& context__,
        std::ostream* pstream__ = 0)
        : prob_grad(0) {
        ctor_body(context__, 0, pstream__);
    }

    model_spolr(stan::io::var_context& context__,
        unsigned int random_seed__,
        std::ostream* pstream__ = 0)
        : prob_grad(0) {
        ctor_body(context__, random_seed__, pstream__);
    }

    void ctor_body(stan::io::var_context& context__,
                   unsigned int random_seed__,
                   std::ostream* pstream__) {
        typedef double local_scalar_t__;

        boost::ecuyer1988 base_rng__ =
          stan::services::util::create_rng(random_seed__, 0);
        (void) base_rng__;  // suppress unused var warning

        current_statement_begin__ = -1;

        static const char* function__ = "model_spolr_namespace::model_spolr";
        (void) function__;  // dummy to suppress unused var warning
        size_t pos__;
        (void) pos__;  // dummy to suppress unused var warning
        std::vector<int> vals_i__;
        std::vector<double> vals_r__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning

        try {
            // initialize data block variables from context__
            current_statement_begin__ = 3;
            context__.validate_dims("data initialization", "N", "int", context__.to_vec());
            N = int(0);
            vals_i__ = context__.vals_i("N");
            pos__ = 0;
            N = vals_i__[pos__++];
            check_greater_or_equal(function__, "N", N, 1);

            current_statement_begin__ = 4;
            validate_non_negative_index("Y", "N", N);
            context__.validate_dims("data initialization", "Y", "int", context__.to_vec(N));
            Y = std::vector<int>(N, int(0));
            vals_i__ = context__.vals_i("Y");
            pos__ = 0;
            size_t Y_k_0_max__ = N;
            for (size_t k_0__ = 0; k_0__ < Y_k_0_max__; ++k_0__) {
                Y[k_0__] = vals_i__[pos__++];
            }

            current_statement_begin__ = 5;
            context__.validate_dims("data initialization", "K", "int", context__.to_vec());
            K = int(0);
            vals_i__ = context__.vals_i("K");
            pos__ = 0;
            K = vals_i__[pos__++];
            check_greater_or_equal(function__, "K", K, 1);

            current_statement_begin__ = 6;
            validate_non_negative_index("X", "N", N);
            validate_non_negative_index("X", "K", K);
            context__.validate_dims("data initialization", "X", "matrix_d", context__.to_vec(N,K));
            X = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>(N, K);
            vals_r__ = context__.vals_r("X");
            pos__ = 0;
            size_t X_j_2_max__ = K;
            size_t X_j_1_max__ = N;
            for (size_t j_2__ = 0; j_2__ < X_j_2_max__; ++j_2__) {
                for (size_t j_1__ = 0; j_1__ < X_j_1_max__; ++j_1__) {
                    X(j_1__, j_2__) = vals_r__[pos__++];
                }
            }

            current_statement_begin__ = 7;
            context__.validate_dims("data initialization", "sd_prior_b", "double", context__.to_vec());
            sd_prior_b = double(0);
            vals_r__ = context__.vals_r("sd_prior_b");
            pos__ = 0;
            sd_prior_b = vals_r__[pos__++];

            current_statement_begin__ = 8;
            context__.validate_dims("data initialization", "ncat", "int", context__.to_vec());
            ncat = int(0);
            vals_i__ = context__.vals_i("ncat");
            pos__ = 0;
            ncat = vals_i__[pos__++];
            check_greater_or_equal(function__, "ncat", ncat, 2);


            // initialize transformed data variables
            // execute transformed data statements

            // validate transformed data

            // validate, set parameter ranges
            num_params_r__ = 0U;
            param_ranges_i__.clear();
            current_statement_begin__ = 11;
            validate_non_negative_index("b", "K", K);
            num_params_r__ += K;
            current_statement_begin__ = 12;
            validate_non_negative_index("Intercept", "(ncat - 1)", (ncat - 1));
            num_params_r__ += (ncat - 1);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }

    ~model_spolr() { }


    void transform_inits(const stan::io::var_context& context__,
                         std::vector<int>& params_i__,
                         std::vector<double>& params_r__,
                         std::ostream* pstream__) const {
        typedef double local_scalar_t__;
        stan::io::writer<double> writer__(params_r__, params_i__);
        size_t pos__;
        (void) pos__; // dummy call to supress warning
        std::vector<double> vals_r__;
        std::vector<int> vals_i__;

        current_statement_begin__ = 11;
        if (!(context__.contains_r("b")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable b missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("b");
        pos__ = 0U;
        validate_non_negative_index("b", "K", K);
        context__.validate_dims("parameter initialization", "b", "vector_d", context__.to_vec(K));
        Eigen::Matrix<double, Eigen::Dynamic, 1> b(K);
        size_t b_j_1_max__ = K;
        for (size_t j_1__ = 0; j_1__ < b_j_1_max__; ++j_1__) {
            b(j_1__) = vals_r__[pos__++];
        }
        try {
            writer__.vector_unconstrain(b);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable b: ") + e.what()), current_statement_begin__, prog_reader__());
        }

        current_statement_begin__ = 12;
        if (!(context__.contains_r("Intercept")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable Intercept missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("Intercept");
        pos__ = 0U;
        validate_non_negative_index("Intercept", "(ncat - 1)", (ncat - 1));
        context__.validate_dims("parameter initialization", "Intercept", "vector_d", context__.to_vec((ncat - 1)));
        Eigen::Matrix<double, Eigen::Dynamic, 1> Intercept((ncat - 1));
        size_t Intercept_j_1_max__ = (ncat - 1);
        for (size_t j_1__ = 0; j_1__ < Intercept_j_1_max__; ++j_1__) {
            Intercept(j_1__) = vals_r__[pos__++];
        }
        try {
            writer__.ordered_unconstrain(Intercept);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable Intercept: ") + e.what()), current_statement_begin__, prog_reader__());
        }

        params_r__ = writer__.data_r();
        params_i__ = writer__.data_i();
    }

    void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double, Eigen::Dynamic, 1>& params_r,
                         std::ostream* pstream__) const {
      std::vector<double> params_r_vec;
      std::vector<int> params_i_vec;
      transform_inits(context, params_i_vec, params_r_vec, pstream__);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r(i) = params_r_vec[i];
    }


    template <bool propto__, bool jacobian__, typename T__>
    T__ log_prob(std::vector<T__>& params_r__,
                 std::vector<int>& params_i__,
                 std::ostream* pstream__ = 0) const {

        typedef T__ local_scalar_t__;

        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // dummy to suppress unused var warning

        T__ lp__(0.0);
        stan::math::accumulator<T__> lp_accum__;
        try {
            stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);

            // model parameters
            current_statement_begin__ = 11;
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, 1> b;
            (void) b;  // dummy to suppress unused var warning
            if (jacobian__)
                b = in__.vector_constrain(K, lp__);
            else
                b = in__.vector_constrain(K);

            current_statement_begin__ = 12;
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, 1> Intercept;
            (void) Intercept;  // dummy to suppress unused var warning
            if (jacobian__)
                Intercept = in__.ordered_constrain((ncat - 1), lp__);
            else
                Intercept = in__.ordered_constrain((ncat - 1));

            // model body
            {
            current_statement_begin__ = 15;
            validate_non_negative_index("mu", "N", N);
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, 1> mu(N);
            stan::math::initialize(mu, DUMMY_VAR__);
            stan::math::fill(mu, DUMMY_VAR__);
            stan::math::assign(mu,multiply(X, b));


            current_statement_begin__ = 17;
            lp_accum__.add(student_t_log(Intercept, 3, 0, 10));
            current_statement_begin__ = 18;
            lp_accum__.add(normal_log(b, 0, sd_prior_b));
            current_statement_begin__ = 20;
            lp_accum__.add(ordered_logistic_log(Y, mu, Intercept));
            }

        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }

        lp_accum__.add(lp__);
        return lp_accum__.sum();

    } // log_prob()

    template <bool propto, bool jacobian, typename T_>
    T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
               std::ostream* pstream = 0) const {
      std::vector<T_> vec_params_r;
      vec_params_r.reserve(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        vec_params_r.push_back(params_r(i));
      std::vector<int> vec_params_i;
      return log_prob<propto,jacobian,T_>(vec_params_r, vec_params_i, pstream);
    }


    void get_param_names(std::vector<std::string>& names__) const {
        names__.resize(0);
        names__.push_back("b");
        names__.push_back("Intercept");
    }


    void get_dims(std::vector<std::vector<size_t> >& dimss__) const {
        dimss__.resize(0);
        std::vector<size_t> dims__;
        dims__.resize(0);
        dims__.push_back(K);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back((ncat - 1));
        dimss__.push_back(dims__);
    }

    template <typename RNG>
    void write_array(RNG& base_rng__,
                     std::vector<double>& params_r__,
                     std::vector<int>& params_i__,
                     std::vector<double>& vars__,
                     bool include_tparams__ = true,
                     bool include_gqs__ = true,
                     std::ostream* pstream__ = 0) const {
        typedef double local_scalar_t__;

        vars__.resize(0);
        stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
        static const char* function__ = "model_spolr_namespace::write_array";
        (void) function__;  // dummy to suppress unused var warning

        // read-transform, write parameters
        Eigen::Matrix<double, Eigen::Dynamic, 1> b = in__.vector_constrain(K);
        size_t b_j_1_max__ = K;
        for (size_t j_1__ = 0; j_1__ < b_j_1_max__; ++j_1__) {
            vars__.push_back(b(j_1__));
        }

        Eigen::Matrix<double, Eigen::Dynamic, 1> Intercept = in__.ordered_constrain((ncat - 1));
        size_t Intercept_j_1_max__ = (ncat - 1);
        for (size_t j_1__ = 0; j_1__ < Intercept_j_1_max__; ++j_1__) {
            vars__.push_back(Intercept(j_1__));
        }

        double lp__ = 0.0;
        (void) lp__;  // dummy to suppress unused var warning
        stan::math::accumulator<double> lp_accum__;

        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning

        if (!include_tparams__ && !include_gqs__) return;

        try {
            if (!include_gqs__ && !include_tparams__) return;
            if (!include_gqs__) return;
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }

    template <typename RNG>
    void write_array(RNG& base_rng,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                     bool include_tparams = true,
                     bool include_gqs = true,
                     std::ostream* pstream = 0) const {
      std::vector<double> params_r_vec(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r_vec[i] = params_r(i);
      std::vector<double> vars_vec;
      std::vector<int> params_i_vec;
      write_array(base_rng, params_r_vec, params_i_vec, vars_vec, include_tparams, include_gqs, pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i)
        vars(i) = vars_vec[i];
    }

    static std::string model_name() {
        return "model_spolr";
    }


    void constrained_param_names(std::vector<std::string>& param_names__,
                                 bool include_tparams__ = true,
                                 bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        size_t b_j_1_max__ = K;
        for (size_t j_1__ = 0; j_1__ < b_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "b" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        size_t Intercept_j_1_max__ = (ncat - 1);
        for (size_t j_1__ = 0; j_1__ < Intercept_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "Intercept" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }

        if (!include_gqs__ && !include_tparams__) return;

        if (include_tparams__) {
        }

        if (!include_gqs__) return;
    }


    void unconstrained_param_names(std::vector<std::string>& param_names__,
                                   bool include_tparams__ = true,
                                   bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        size_t b_j_1_max__ = K;
        for (size_t j_1__ = 0; j_1__ < b_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "b" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        size_t Intercept_j_1_max__ = (ncat - 1);
        for (size_t j_1__ = 0; j_1__ < Intercept_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "Intercept" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }

        if (!include_gqs__ && !include_tparams__) return;

        if (include_tparams__) {
        }

        if (!include_gqs__) return;
    }

}; // model

}  // namespace

typedef model_spolr_namespace::model_spolr stan_model;


#endif
