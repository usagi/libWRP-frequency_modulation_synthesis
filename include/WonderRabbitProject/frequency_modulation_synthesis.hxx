#include <ratio>
#include <type_traits>
#include <cmath>
#include <random>
#include <vector>

namespace WonderRabbitProject
{
  namespace frequency_modulation_synthesis
  {
    using default_float_type = long double;
    
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
          return float_type(1) - (time - attack) * (sustain - float_type(1)) / decay;
        
        return sustain - (time - attack) * ( -sustain ) / release;
      }
      
      const bool end_of_note(const float_type time) const
      {
        return time > attack + decay + release;
      }
      
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
        
        virtual const float_type operate(const float_type time) const = 0;
        
        const envelope_type& get_envelope() const { return envelope; }
        const float_type get_amplitude() const { return amplitude; }
        const float_type get_frequency() const { return frequency; }
        
        void set_envelope(const envelope_type& envelope_) { envelope = envelope_; }
        void set_envelope(envelope_type&& envelope_) { envelope = std::move(envelope_); }
        void set_amplitude(const float_type amplitude_) { amplitude = amplitude_; }
        void set_frequency(const float_type frequency_) { frequency = frequency_; }
        
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
          virtual const float_type operate(const float_type time) const override
          {
            constexpr auto two_pi = float_type(2) * std::atan(float_type(-1));
            
            return base_type::envelope.calc_amplitude(time)
                * base_type::amplitude
                * std::sin(two_pi * base_type::frequency * time)
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
          
          virtual const float_type operate(const float_type time) const override
          {
            constexpr auto two_pi = float_type(2) * std::atan(float_type(-1));
            
            return base_type::envelope.calc_amplitude(time)
                * base_type::amplitude
                //* ( std::sin(two_pi * base_type::frequency * time) >= 0 ? 1 : -1);
                * (4.f * std::floor(time * base_type::frequency) - 2.f * std::floor(2.f * time * base_type::frequency) + 1.f)
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
          
          virtual const float_type operate(const float_type time) const override
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
          {
          }
          
          virtual const float_type operate(const float_type time) const override
          {
            constexpr auto pi = std::atan(float_type(-1));
            
            return base_type::envelope.calc_amplitude(time)
                * base_type::amplitude
                * float_type(2) * ( time * base_type::frequency - std::floor(float_type(0.5) + time * base_type::frequency) )
                ;
          }
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
          
          virtual const float_type operate(const float_type time) const override
          {
            constexpr auto pi = std::atan(float_type(-1));
            
            return base_type::envelope.calc_amplitude(time)
                * base_type::amplitude
                * std::max(float_type(-1), std::min(float_type(1), trapezoid_amplitude * (float_type(2) / pi) * std::asin(std::sin(time * base_type::frequency * float_type(2) * pi)) ))
                ;
          }
          
          void set_trapezoid_amplitude(const float_type trapezoid_amplitude_)
          { trapezoid_amplitude = trapezoid_amplitude_; }
          
          const float_type get_trapezoid_amplitude() const
          { return trapezoid_amplitude; }
          
        protected:
          float_type trapezoid_amplitude;
        };
        
      }
      
      namespace noise
      {
        template<class T_float = default_float_type, class T_distribution = std::uniform_real_distribution<T_float>, class T_rng = std::default_random_engine, class T_envelope = envelope_t<T_float>>
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
          {
          }
          
          virtual const float_type operate(const float_type time) const override
          {
            constexpr auto pi = std::atan(float_type(-1));
            
            return base_type::envelope.calc_amplitude(time)
                * base_type::amplitude
                * distribution(rng)
                ;
          }
          
          typename distribution_type::param_type get_distribution_param() const
          { return distribution.param(); }
          
          void set_distribution_param(const typename distribution_type::param_type& param)
          { distribution.param(param); }
          
        protected:
          distribution_type distribution;
          rng_type rng;
        };
        
        template<class T_float = default_float_type, class T_distribution = std::uniform_real_distribution<T_float>, class T_rng = std::default_random_engine, class T_envelope = envelope_t<T_float>>
        class pink_t: public white_t<T_float, T_distribution, T_rng, T_envelope>
        {
        public:
          using float_type = T_float;
          using envelope_type = T_envelope;
          
          using rng_type = T_rng;
          using distribution_type = T_distribution;
          
          using base_type = operator_t<float_type, envelope_type>;
          using parent_type = white_t<float_type, rng_type, distribution_type, envelope_type>;
          using this_type = pink_t<float_type, rng_type, distribution_type, envelope_type>;
          
          explicit
          pink_t(const float_type pink_alpha_, const float_type pink_step_ , const envelope_type& envelope_ = envelope_type(), float_type amplitude_ = 1, float_type frequency_ = constants::concert_pitch::standard)
            : base_type(envelope_, amplitude_, frequency_)
            , pink_alpha(pink_alpha_)
            , pink_step(pink_step_)
          {
          }
          
          // http://sampo.kapsi.fi/PinkNoise/
          virtual const float_type operate(const float_type time) const override
          {
            constexpr auto pi = std::atan(float_type(-1));
            
            auto pink_value = distribution(rng);
            
            for(uint n = 0; n < pink_step; ++n)
              pink_value *= (float_type(n - 1) - pink_alpha * float_type(.5)) / n ;
            
            return base_type::envelope.calc_amplitude(time)
                * base_type::amplitude
                * pink_value
                ;
          }
          
          typename distribution_type::param_type get_distribution_param() const
          { return distribution.param(); }
          
          void set_distribution_param(const typename distribution_type::param_type& param)
          { distribution.param(param); }
          
        protected:
          distribution_type distribution;
          rng_type rng;
          float_type pink_alpha;
          uint pink_step;
        };
        
      }
    }
    
    class algorithm
    {
      
    };
    
    namespace programs
    {
      
    }
    
  }
}