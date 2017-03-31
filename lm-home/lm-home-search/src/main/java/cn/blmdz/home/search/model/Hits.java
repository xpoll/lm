package cn.blmdz.home.search.model;

import java.io.Serializable;
import java.util.List;

public class Hits implements Serializable {
   private static final long serialVersionUID = -8033128358999935071L;
   private Long total;
   private Float max_score;
   private List hits;

   public Long getTotal() {
      return this.total;
   }

   public Float getMax_score() {
      return this.max_score;
   }

   public List getHits() {
      return this.hits;
   }

   public void setTotal(Long total) {
      this.total = total;
   }

   public void setMax_score(Float max_score) {
      this.max_score = max_score;
   }

   public void setHits(List hits) {
      this.hits = hits;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof Hits)) {
         return false;
      } else {
         Hits other = (Hits)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$total = this.getTotal();
            Object other$total = other.getTotal();
            if(this$total == null) {
               if(other$total != null) {
                  return false;
               }
            } else if(!this$total.equals(other$total)) {
               return false;
            }

            Object this$max_score = this.getMax_score();
            Object other$max_score = other.getMax_score();
            if(this$max_score == null) {
               if(other$max_score != null) {
                  return false;
               }
            } else if(!this$max_score.equals(other$max_score)) {
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

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof Hits;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $total = this.getTotal();
      result = result * 59 + ($total == null?0:$total.hashCode());
      Object $max_score = this.getMax_score();
      result = result * 59 + ($max_score == null?0:$max_score.hashCode());
      Object $hits = this.getHits();
      result = result * 59 + ($hits == null?0:$hits.hashCode());
      return result;
   }

   public String toString() {
      return "Hits(total=" + this.getTotal() + ", max_score=" + this.getMax_score() + ", hits=" + this.getHits() + ")";
   }
}
