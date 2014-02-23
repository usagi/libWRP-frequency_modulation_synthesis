#include <ratio>
#include <type_traits>
#include <cmath>
#include <random>
#include <unordered_map>
#include <list>
#include <memory>
#include <algorithm>
#include <stdexcept>

namespace WonderRabbitProject
{
  namespace frequency_modulation_synthesis
  {
    using default_float_type = long double;
    using default_random_engine = std::mt19937;
    
    constexpr uint32_t default_target_sampling_rate = 48000;
    
    namespace constants
    {
      namespace concert_pitch
      {
        // http://ja.wikipedia.org/wiki/%E9%9F%B3%E9%AB%98
        constexpr default_float_type old_Japanese_before_1948     = 435;
        constexpr default_float_type Japanese_piano_and_orchestra = 442.5;
        
        // http://en.wikipedia.org/wiki/Concert_pitch
        constexpr default_float_type in_1720_Johann_Sebastian_Bach  = 380;
        constexpr default_float_type in_1720_Leipzig_and_Weimar     = 480;
        constexpr default_float_type in_1740_George_Frideric_Handel = 422.5;
        constexpr default_float_type in_1780                        = 409;
        constexpr default_float_type in_1815_Dresden_opera_house = 423.2;
        constexpr default_float_type in_1816_Dresden_opera_house = 435;
        constexpr default_float_type in_1815_La_Scala_Milan      = 451;
        constexpr default_float_type in_1859_French_goverment = 435;
        constexpr default_float_type scientific = 430.54;
        constexpr default_float_type Giuseppe_Verdi = 432;
        constexpr default_float_type in_19th_British_old_philharmonic = 452;
        constexpr default_float_type in_1896_British_old_philharmonic = 439;
        constexpr default_float_type in_1895_Promenade_Concerts_59F    = 439;
        constexpr default_float_type in_1895_Promenade_Concerts_heated = 435.5;
        constexpr default_float_type in_1896_England_Philharmonic_Society_low_68F  = 439;
        constexpr default_float_type in_1896_England_Philharmonic_Society_high_60F = 452.4;
        constexpr default_float_type in_1975_ISO16 = 440;
        constexpr default_float_type New_York_Philharmonic_and_Boston_Symphony_Orchestr = 442;
        constexpr default_float_type centinental_Europe = 443;
        constexpr default_float_type Berliner_Philharmoniker     = 443;
        constexpr default_float_type old_Berliner_Philharmoniker = 445;
        constexpr default_float_type Baroque = 415;
        
        constexpr default_float_type standard = in_1975_ISO16;
      }
      
      constexpr default_float_type pi = default_float_type(4) * std::atan(default_float_type(1));
      constexpr default_float_type two_pi = pi * 2;
    }
    
    // http://www.phys.unsw.edu.au/jw/notes.html
    template<class T = default_float_type>
    static constexpr T calc_frequency(int MIDI_number, T A4_frequency = constants::concert_pitch::standard)
    { return std::exp2(T(MIDI_number - 69) / T(12)) * A4_frequency; }
    
    template<class T_float = default_float_type>
    class envelope_t
    {
    public:
      using float_type = T_float;
      
      explicit
      envelope_t(float_type attack_ = 0.0, float_type decay_ = 1.0, float_type sustain_ = 1.0, float_type release_ = 0.0)
        : attack(attack_)
        , decay(decay_)
        , sustain(sustain_)
        , release(release_)
      {
      }
      
      // http://ja.wikipedia.org/wiki/ADSR
      const float_type calc_amplitude(const float_type time) const
      {
        if(time < attack)
          return time / attack;
        
        if(time < attack + decay)
          return float_type(1) + (time - attack) * (sustain - float_type(1)) / decay;
        
        if(time < attack + decay + release)
          return sustain + (time - attack - decay) * ( -sustain ) / release;
        
        return 0;
      }
      
      const bool end_of_note(const float_type time) const
      { return time > get_total_time(); }
      
      const float_type get_total_time() const
      { return attack + decay + release; }
      
      const float_type get_attack() const { return attack; }
      const float_type get_decay() const { return decay; }
      const float_type get_sustain() const { return sustain; }
      const float_type get_release() const { return release; }
      
      void set_attack(const float_type attack_) { attack = attack_; }
      void set_decay(const float_type decay_) { decay = decay_; }
      void set_sustain(const float_type sustain_) { sustain = sustain_; }
      void set_release(const float_type release_) { release = release_; }
      
      void set_params(const float_type attack_, const float_type decay_, const float_type sustain_, const float_type release_)
      {
        attack  = attack_;
        decay   = decay_;
        sustain = sustain_;
        release = release_;
      }
      
    protected:
      float_type attack;
      float_type decay;
      float_type sustain;
      float_type release;
    };
    
    namespace operators
    {
      template<class T_float = default_float_type, class T_envelope = envelope_t<T_float>>
      class operator_t
      {
      public:
        using float_type = T_float;
        using envelope_type = T_envelope;
        
        using this_type = operator_t<float_type, envelope_type>;
        
        explicit
        operator_t(const envelope_type& envelope_ = envelope_type(), float_type amplitude_ = 1, float_type frequency_ = constants::concert_pitch::standard)
          : envelope(envelope_)
          , amplitude(amplitude_)
          , frequency(frequency_)
        {
        }
        
        virtual const float_type operate(const float_type time) = 0;
        
        virtual const envelope_type& get_envelope() const { return envelope; }
        virtual const typename envelope_type::float_type get_envelope_attack() const { return envelope.get_attack(); }
        virtual const typename envelope_type::float_type get_envelope_decay() const { return envelope.get_decay(); }
        virtual const typename envelope_type::float_type get_envelope_sustain() const { return envelope.get_sustain(); }
        virtual const typename envelope_type::float_type get_envelope_release() const { return envelope.get_release(); }
        virtual const float_type get_envelope_total_time() const { return envelope.get_total_time(); }
        
        virtual const float_type get_amplitude() const { return amplitude; }
        virtual const float_type get_frequency() const { return frequency; }
        
        virtual this_type& set_envelope(const envelope_type& envelope_) { envelope = envelope_; return *this; }
        virtual this_type& set_envelope(envelope_type&& envelope_) { envelope = std::move(envelope_); return *this; }
        virtual this_type& set_envelope_params
        ( const typename envelope_type::float_type attack
        , const typename envelope_type::float_type decay
        , const typename envelope_type::float_type sustain
        , const typename envelope_type::float_type release
        ) { envelope.set_params(attack, decay, sustain, release); return *this; }
        virtual this_type& set_envelope_attack(const typename envelope_type::float_type attack) { envelope.set_attack(attack); return *this; }
        virtual this_type& set_envelope_decay(const typename envelope_type::float_type decay) { envelope.set_decay(decay); return *this; }
        virtual this_type& set_envelope_sustain(const typename envelope_type::float_type sustain) { envelope.set_sustain(sustain); return *this; }
        virtual this_type& set_envelope_release(const typename envelope_type::float_type release) { envelope.set_release(release); return *this; }
        virtual this_type& set_amplitude(const float_type amplitude_) { amplitude = amplitude_; return *this; }
        virtual this_type& set_frequency(const float_type frequency_) { frequency = frequency_; return *this; }
        
      protected:
        envelope_type envelope;
        float_type amplitude;
        float_type frequency;
      };
        
      namespace wave
      {
        template<class T_float = default_float_type, class T_envelope = envelope_t<T_float>>
        class sine_t: public operator_t<T_float, T_envelope>
        {
        public:
          using float_type = T_float;
          using envelope_type = T_envelope;
          
          using base_type = operator_t<float_type, envelope_type>;
          using this_type = sine_t<float_type, envelope_type>;
          
          explicit
          sine_t(const envelope_type& envelope_ = envelope_type(), float_type amplitude_ = 1, float_type frequency_ = constants::concert_pitch::standard)
            : base_type(envelope_, amplitude_, frequency_)
          {
          }
          
          // http://ja.wikipedia.org/wiki/FM%E9%9F%B3%E6%BA%90
          virtual const float_type operate(const float_type time) override
          {
            return base_type::envelope.calc_amplitude(time)
                * base_type::amplitude
                * std::sin(constants::two_pi * base_type::frequency * time)
                ;
          }
        };
        
        template<class T_float = default_float_type, class T_envelope = envelope_t<T_float>>
        class square_t: public operator_t<T_float, T_envelope>
        {
        public:
          using float_type = T_float;
          using envelope_type = T_envelope;
          
          using base_type = operator_t<float_type, envelope_type>;
          using this_type = square_t<float_type, envelope_type>;
          
          explicit
          square_t(const envelope_type& envelope_ = envelope_type(), float_type amplitude_ = 1, float_type frequency_ = constants::concert_pitch::standard)
            : base_type(envelope_, amplitude_, frequency_)
          {
          }
          
          virtual const float_type operate(const float_type time) override
          {
            return base_type::envelope.calc_amplitude(time)
                * base_type::amplitude
                * ( std::sin(constants::two_pi * base_type::frequency * time) >= 0 ? 1 : -1);
                ;
          }
        };
        
        template<class T_float = default_float_type, class T_envelope = envelope_t<T_float>>
        class triangle_t: public operator_t<T_float, T_envelope>
        {
        public:
          using float_type = T_float;
          using envelope_type = T_envelope;
          
          using base_type = operator_t<float_type, envelope_type>;
          using this_type = triangle_t<float_type, envelope_type>;
          
          explicit
          triangle_t(const envelope_type& envelope_ = envelope_type(), float_type amplitude_ = 1, float_type frequency_ = constants::concert_pitch::standard)
            : base_type(envelope_, amplitude_, frequency_)
          {
          }
          
          virtual const float_type operate(const float_type time) override
          {
            constexpr auto pi = std::atan(float_type(-1));
            
            return base_type::envelope.calc_amplitude(time)
                * base_type::amplitude
                * (float_type(2) / pi) * std::asin(std::sin(time * base_type::frequency * float_type(2) * pi))
                ;
          }
        };
      
        template<class T_float = default_float_type, class T_envelope = envelope_t<T_float>>
        class sawtooth_t: public operator_t<T_float, T_envelope>
        {
        public:
          using float_type = T_float;
          using envelope_type = T_envelope;
          
          using base_type = operator_t<float_type, envelope_type>;
          using this_type = sawtooth_t<float_type, envelope_type>;
          
          explicit
          sawtooth_t(const envelope_type& envelope_ = envelope_type(), float_type amplitude_ = 1, float_type frequency_ = constants::concert_pitch::standard)
            : base_type(envelope_, amplitude_, frequency_)
            , target_sampling_rate(default_target_sampling_rate)
          {
          }
          
          virtual const float_type operate(const float_type time) override
          {
            constexpr auto pi = std::atan(float_type(-1));
            
            float_type sawtooth_value = 0;
            
            for(uint n = 1, m = calc_fourier_step(); n < m; ++n)
              sawtooth_value += std::sin( float_type(n) * time * base_type::frequency * constants::two_pi) / float_type(n);
            
            sawtooth_value *= float_type(2) / constants::pi;
            
            return base_type::envelope.calc_amplitude(time)
                * base_type::amplitude
                * sawtooth_value
                ;
          }
          
          this_type& set_target_sampling_rate(const uint32_t target_sampling_rate_)
          { target_sampling_rate = target_sampling_rate_; return *this; }
          
          uint32_t get_target_sampling_rate() const
          { return target_sampling_rate; }
          
        protected:
          uint calc_fourier_step()
          { return static_cast<uint>(std::ceil(std::log2(target_sampling_rate / 2 / base_type::frequency))); }
          
          uint32_t target_sampling_rate;
        };
        
        template<class T_float = default_float_type, class T_envelope = envelope_t<T_float>>
        class trapezoid_t: public operator_t<T_float, T_envelope>
        {
        public:
          using float_type = T_float;
          using envelope_type = T_envelope;
          
          using base_type = operator_t<float_type, envelope_type>;
          using this_type = trapezoid_t<float_type, envelope_type>;
          
          explicit
          trapezoid_t(const envelope_type& envelope_ = envelope_type(), float_type amplitude_ = 1, float_type frequency_ = constants::concert_pitch::standard, const float_type trapezoid_amplitude_ = 2)
            : base_type(envelope_, amplitude_, frequency_)
            , trapezoid_amplitude(trapezoid_amplitude_)
          {
          }
          
          virtual const float_type operate(const float_type time) override
          {
            constexpr auto pi = std::atan(float_type(-1));
            
            return base_type::envelope.calc_amplitude(time)
                * base_type::amplitude
                * std::max(float_type(-1), std::min(float_type(1), trapezoid_amplitude * (float_type(2) / pi) * std::asin(std::sin(time * base_type::frequency * float_type(2) * pi)) ))
                ;
          }
          
          this_type& set_trapezoid_amplitude(const float_type trapezoid_amplitude_)
          { trapezoid_amplitude = trapezoid_amplitude_; return *this; }
          
          const float_type get_trapezoid_amplitude() const
          { return trapezoid_amplitude; }
          
        protected:
          float_type trapezoid_amplitude;
        };
        
      }
      
      namespace noise
      {
        template<class T_float = default_float_type, class T_distribution = std::uniform_real_distribution<T_float>, class T_rng = default_random_engine, class T_envelope = envelope_t<T_float>>
        class white_t: public operator_t<T_float, T_envelope>
        {
        public:
          using float_type = T_float;
          using envelope_type = T_envelope;
          
          using rng_type = T_rng;
          using distribution_type = T_distribution;
          
          using base_type = operator_t<float_type, envelope_type>;
          using this_type = white_t<float_type, rng_type, distribution_type, envelope_type>;
          
          explicit
          white_t(const envelope_type& envelope_ = envelope_type(), float_type amplitude_ = 1, float_type frequency_ = constants::concert_pitch::standard)
            : base_type(envelope_, amplitude_, frequency_)
            , distribution(-1, 1)
          {
          }
          
          virtual const float_type operate(const float_type time) override
          {
            constexpr auto pi = std::atan(float_type(-1));
            
            return base_type::envelope.calc_amplitude(time)
                * base_type::amplitude
                * distribution(rng)
                ;
          }
          
          typename distribution_type::param_type get_distribution_param() const
          { return distribution.param(); }
          
          this_type& set_distribution_param(const typename distribution_type::param_type& param)
          { distribution.param(param); return *this; }
          
        protected:
          distribution_type distribution;
          rng_type rng;
        };
        
        template<class T_float = default_float_type, class T_distribution = std::uniform_real_distribution<T_float>, class T_rng = default_random_engine, class T_envelope = envelope_t<T_float>>
        class pink_t: public white_t<T_float, T_distribution, T_rng, T_envelope>
        {
        public:
          using float_type = T_float;
          using envelope_type = T_envelope;
          
          using rng_type = T_rng;
          using distribution_type = T_distribution;
          
          using base_type = operator_t<float_type, envelope_type>;
          using parent_type = white_t<T_float, T_distribution, T_rng, T_envelope>;
          using this_type = pink_t<float_type, rng_type, distribution_type, envelope_type>;
          
          explicit
          pink_t(const float_type pink_alpha_ = 1, const float_type pink_step_ = 8, const envelope_type& envelope_ = envelope_type(), float_type amplitude_ = 1, float_type frequency_ = constants::concert_pitch::standard)
            : parent_type(envelope_, amplitude_, frequency_)
            , distribution(-1, 1)
            , pink_alpha(pink_alpha_)
            , pink_step(pink_step_)
          {
          }
          
          // http://sampo.kapsi.fi/PinkNoise/
          virtual const float_type operate(const float_type time) override
          {
            constexpr auto pi = std::atan(float_type(-1));
            
            auto pink_value = distribution(rng);
            auto a = 1;
            
            for(uint n = 1; n < pink_step; ++n)
            {
              a *= (float_type(n - 1) - pink_alpha * float_type(.5)) / n;
              pink_value -= a * pink_value;
            }
            std::cout << pink_value << "\n";
            return base_type::envelope.calc_amplitude(time)
                * base_type::amplitude
                * pink_value
                ;
          }
          
          typename distribution_type::param_type get_distribution_param() const
          { return distribution.param(); }
          
          this_type& set_distribution_param(const typename distribution_type::param_type& param)
          { distribution.param(param); return *this; }
          
        protected:
          distribution_type distribution;
          rng_type rng;
          float_type pink_alpha;
          uint pink_step;
        };
        
      }
    }
    
    template<class T_base_operator = operators::operator_t<>>
    class algorithm_t
    {
    public:
      using base_operator_type = T_base_operator;
      using float_type = typename T_base_operator::float_type;
      
      using operator_type = std::shared_ptr<base_operator_type>;
      using operators_type = std::unordered_map<std::string, operator_type>;
      
      using path_type = std::list<operator_type>;
      using paths_type = std::unordered_map<std::string, path_type>;
      
      using timers_type = std::list<float_type>;
      
      template<class T>
      //std::shared_ptr<T> 
      T&
      emplace_operator(std::string&& name, T&& op)
      {
        const auto r = operators.emplace(std::move(name), std::make_shared<T>( std::move(op) ));
        
        if(!r.second)
          throw std::runtime_error("operators.emplace to failed");
        
        //return *reinterpret_cast<std::shared_ptr<T>*>( & r.first->second );
        return *static_cast<T*>( r.first->second.get() );
      }
      
      template<class T = base_operator_type>
      T&
      get_operator(const std::string& operator_name)
      {
        const auto i = operators.find(operator_name);
        if( i == std::end(operators) )
          throw std::runtime_error("operator_name is not found in operators");
        
        return *static_cast<T*>( i->second.get() );
      }
      
      void remove_operator(const std::string& operator_name)
      {
        const auto i = operators.find(operator_name);
        if( i == std::end(operators) )
          throw std::runtime_error("operator_name is not found in operators");
        
        // remove path included the operator
        {
          const auto& op = i->second;
          std::vector<std::string> remove_path_names;
          
          //for(const auto& path : paths)
          for(auto i = paths.cbegin(); i < paths.cend(); ++i)
            for(const auto& path_op : i->second)
              if(path_op == op)
              {
                remove_path_names.emplace_back(i->first);
                break;
              }
        }
        
        // remove operator
        operators.erase(operator_name);
      }
      
      void add_path(const std::string& path_name, const std::string& operator_name)
      {
        if( operators.find(operator_name) == std::end(operators) )
          throw std::runtime_error("operator_name not found in operators");
        
        if( paths.find(path_name) == std::end(paths) )
          paths.emplace(path_name, path_type() );
        
        paths[path_name].emplace_back( operators[operator_name] );
      }
      
      const std::vector<std::string> get_path_operator_names(const std::string path_name) const
      {
        std::vector<std::string> r;
        
        if( paths.find(path_name) == std::end(paths) )
          throw std::runtime_error("path_name not found in paths");
        
        for(const auto& op : paths.at(path_name))
          for(const auto& i : operators)
            if(i.second == op)
              r.emplace_back(i.first);
        
        return r;
      }
      
      void remove_path(const std::string& path_name)
      { paths.erase(path_name); }
      
      const float_type synthesize(const float_type time) const
      {
        float_type r;
        
        for(auto path : paths)
        {
          if(path.second.empty())
            continue;
          
          auto i = path.second.cbegin();
          auto r_path = (*i)->operate(time);
          
          while(++i != path.second.cend())
            r_path = (*i)->operate(r_path);
          
          r += r_path;
        }
        
        return r;
      }
      
      void add_note()
      {
        timers.emplace_back(0);
      }
      
      const float_type play_note() const
      {
        float_type r;
        
        for(const auto timer : timers)
          r += synthesize(timer);
        
        return r;
      }
      
      void update_note(const float_type delta_time)
      {
        for(auto i = timers.begin(); i != timers.end(); ++i)
        {
          *i += delta_time;
          if(std::all_of(paths.cbegin(), paths.cend(), [&i](const base_operator_type& op){ return op.end_of_note(*i); }))
            i = timers.erase(i) - 1;
        }
      }
      
    protected:
      operators_type operators;
      paths_type paths;
      timers_type timers;
    };
    
  }
}