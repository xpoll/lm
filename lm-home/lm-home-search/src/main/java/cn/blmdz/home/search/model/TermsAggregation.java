package cn.blmdz.home.search.model;

import java.io.Serializable;
import java.util.List;

public class TermsAggregation implements Serializable {
   private static final long serialVersionUID = -4914786763630433807L;
   private Long doc_count_error_upper_bound;
   private Long sum_other_doc_count;
   private List buckets;

   public Long getDoc_count_error_upper_bound() {
      return this.doc_count_error_upper_bound;
   }

   public Long getSum_other_doc_count() {
      return this.sum_other_doc_count;
   }

   public List getBuckets() {
      return this.buckets;
   }

   public void setDoc_count_error_upper_bound(Long doc_count_error_upper_bound) {
      this.doc_count_error_upper_bound = doc_count_error_upper_bound;
   }

   public void setSum_other_doc_count(Long sum_other_doc_count) {
      this.sum_other_doc_count = sum_other_doc_count;
   }

   public void setBuckets(List buckets) {
      this.buckets = buckets;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof TermsAggregation)) {
         return false;
      } else {
         TermsAggregation other = (TermsAggregation)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$doc_count_error_upper_bound = this.getDoc_count_error_upper_bound();
            Object other$doc_count_error_upper_bound = other.getDoc_count_error_upper_bound();
            if(this$doc_count_error_upper_bound == null) {
               if(other$doc_count_error_upper_bound != null) {
                  return false;
               }
            } else if(!this$doc_count_error_upper_bound.equals(other$doc_count_error_upper_bound)) {
               return false;
            }

            Object this$sum_other_doc_count = this.getSum_other_doc_count();
            Object other$sum_other_doc_count = other.getSum_other_doc_count();
            if(this$sum_other_doc_count == null) {
               if(other$sum_other_doc_count != null) {
                  return false;
               }
            } else if(!this$sum_other_doc_count.equals(other$sum_other_doc_count)) {
               return false;
            }

            Object this$buckets = this.getBuckets();
            Object other$buckets = other.getBuckets();
            if(this$buckets == null) {
               if(other$buckets != null) {
                  return false;
               }
            } else if(!this$buckets.equals(other$buckets)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof TermsAggregation;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $doc_count_error_upper_bound = this.getDoc_count_error_upper_bound();
      result = result * 59 + ($doc_count_error_upper_bound == null?0:$doc_count_error_upper_bound.hashCode());
      Object $sum_other_doc_count = this.getSum_other_doc_count();
      result = result * 59 + ($sum_other_doc_count == null?0:$sum_other_doc_count.hashCode());
      Object $buckets = this.getBuckets();
      result = result * 59 + ($buckets == null?0:$buckets.hashCode());
      return result;
   }

   public String toString() {
      return "TermsAggregation(doc_count_error_upper_bound=" + this.getDoc_count_error_upper_bound() + ", sum_other_doc_count=" + this.getSum_other_doc_count() + ", buckets=" + this.getBuckets() + ")";
   }
}
