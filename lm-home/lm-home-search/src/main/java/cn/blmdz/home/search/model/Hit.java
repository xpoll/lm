package cn.blmdz.home.search.model;

import java.io.Serializable;
import java.util.Map;

public class Hit implements Serializable {
   private static final long serialVersionUID = 964771673077104982L;
   private String _index;
   private String _type;
   private String _id;
   private Float _score;
   private Map _source;
   private Map highlight;

   public String get_index() {
      return this._index;
   }

   public String get_type() {
      return this._type;
   }

   public String get_id() {
      return this._id;
   }

   public Float get_score() {
      return this._score;
   }

   public Map get_source() {
      return this._source;
   }

   public Map getHighlight() {
      return this.highlight;
   }

   public void set_index(String _index) {
      this._index = _index;
   }

   public void set_type(String _type) {
      this._type = _type;
   }

   public void set_id(String _id) {
      this._id = _id;
   }

   public void set_score(Float _score) {
      this._score = _score;
   }

   public void set_source(Map _source) {
      this._source = _source;
   }

   public void setHighlight(Map highlight) {
      this.highlight = highlight;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof Hit)) {
         return false;
      } else {
         Hit other = (Hit)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$_index = this.get_index();
            Object other$_index = other.get_index();
            if(this$_index == null) {
               if(other$_index != null) {
                  return false;
               }
            } else if(!this$_index.equals(other$_index)) {
               return false;
            }

            Object this$_type = this.get_type();
            Object other$_type = other.get_type();
            if(this$_type == null) {
               if(other$_type != null) {
                  return false;
               }
            } else if(!this$_type.equals(other$_type)) {
               return false;
            }

            Object this$_id = this.get_id();
            Object other$_id = other.get_id();
            if(this$_id == null) {
               if(other$_id != null) {
                  return false;
               }
            } else if(!this$_id.equals(other$_id)) {
               return false;
            }

            Object this$_score = this.get_score();
            Object other$_score = other.get_score();
            if(this$_score == null) {
               if(other$_score != null) {
                  return false;
               }
            } else if(!this$_score.equals(other$_score)) {
               return false;
            }

            Object this$_source = this.get_source();
            Object other$_source = other.get_source();
            if(this$_source == null) {
               if(other$_source != null) {
                  return false;
               }
            } else if(!this$_source.equals(other$_source)) {
               return false;
            }

            Object this$highlight = this.getHighlight();
            Object other$highlight = other.getHighlight();
            if(this$highlight == null) {
               if(other$highlight != null) {
                  return false;
               }
            } else if(!this$highlight.equals(other$highlight)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof Hit;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $_index = this.get_index();
      result = result * 59 + ($_index == null?0:$_index.hashCode());
      Object $_type = this.get_type();
      result = result * 59 + ($_type == null?0:$_type.hashCode());
      Object $_id = this.get_id();
      result = result * 59 + ($_id == null?0:$_id.hashCode());
      Object $_score = this.get_score();
      result = result * 59 + ($_score == null?0:$_score.hashCode());
      Object $_source = this.get_source();
      result = result * 59 + ($_source == null?0:$_source.hashCode());
      Object $highlight = this.getHighlight();
      result = result * 59 + ($highlight == null?0:$highlight.hashCode());
      return result;
   }

   public String toString() {
      return "Hit(_index=" + this.get_index() + ", _type=" + this.get_type() + ", _id=" + this.get_id() + ", _score=" + this.get_score() + ", _source=" + this.get_source() + ", highlight=" + this.getHighlight() + ")";
   }
}
