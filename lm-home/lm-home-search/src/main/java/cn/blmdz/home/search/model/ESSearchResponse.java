package cn.blmdz.home.search.model;

import java.io.Serializable;
import java.util.Map;

import cn.blmdz.home.search.model.Hits;
import cn.blmdz.home.search.model.Shards;

public class ESSearchResponse implements Serializable {
   private static final long serialVersionUID = 1920380772443937322L;
   private Integer took;
   private Boolean timed_out;
   private Shards _shards;
   private Hits hits;
   private Map aggregations;

   public Integer getTook() {
      return this.took;
   }

   public Boolean getTimed_out() {
      return this.timed_out;
   }

   public Shards get_shards() {
      return this._shards;
   }

   public Hits getHits() {
      return this.hits;
   }

   public Map getAggregations() {
      return this.aggregations;
   }

   public void setTook(Integer took) {
      this.took = took;
   }

   public void setTimed_out(Boolean timed_out) {
      this.timed_out = timed_out;
   }

   public void set_shards(Shards _shards) {
      this._shards = _shards;
   }

   public void setHits(Hits hits) {
      this.hits = hits;
   }

   public void setAggregations(Map aggregations) {
      this.aggregations = aggregations;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof ESSearchResponse)) {
         return false;
      } else {
         ESSearchResponse other = (ESSearchResponse)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$took = this.getTook();
            Object other$took = other.getTook();
            if(this$took == null) {
               if(other$took != null) {
                  return false;
               }
            } else if(!this$took.equals(other$took)) {
               return false;
            }

            Object this$timed_out = this.getTimed_out();
            Object other$timed_out = other.getTimed_out();
            if(this$timed_out == null) {
               if(other$timed_out != null) {
                  return false;
               }
            } else if(!this$timed_out.equals(other$timed_out)) {
               return false;
            }

            Object this$_shards = this.get_shards();
            Object other$_shards = other.get_shards();
            if(this$_shards == null) {
               if(other$_shards != null) {
                  return false;
               }
            } else if(!this$_shards.equals(other$_shards)) {
               return false;
            }

            Object this$hits = this.getHits();
            Object other$hits = other.getHits();
            if(this$hits == null) {
               if(other$hits != null) {
                  return false;
               }
            } else if(!this$hits.equals(other$hits)) {
               return false;
            }

            Object this$aggregations = this.getAggregations();
            Object other$aggregations = other.getAggregations();
            if(this$aggregations == null) {
               if(other$aggregations != null) {
                  return false;
               }
            } else if(!this$aggregations.equals(other$aggregations)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof ESSearchResponse;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $took = this.getTook();
      result = result * 59 + ($took == null?0:$took.hashCode());
      Object $timed_out = this.getTimed_out();
      result = result * 59 + ($timed_out == null?0:$timed_out.hashCode());
      Object $_shards = this.get_shards();
      result = result * 59 + ($_shards == null?0:$_shards.hashCode());
      Object $hits = this.getHits();
      result = result * 59 + ($hits == null?0:$hits.hashCode());
      Object $aggregations = this.getAggregations();
      result = result * 59 + ($aggregations == null?0:$aggregations.hashCode());
      return result;
   }

   public String toString() {
      return "ESSearchResponse(took=" + this.getTook() + ", timed_out=" + this.getTimed_out() + ", _shards=" + this.get_shards() + ", hits=" + this.getHits() + ", aggregations=" + this.getAggregations() + ")";
   }
}
