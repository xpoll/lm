package cn.blmdz.home.search.api.query;

import java.io.Serializable;
import java.util.List;

import cn.blmdz.home.search.api.query.CriteriasBuilder;
import cn.blmdz.home.search.api.query.Keyword;

public class Criterias implements Serializable {
   private static final long serialVersionUID = 8847109666993211943L;
   private int from;
   private int size;
   private boolean hasFilter;
   private boolean hasSort;
   private boolean hasAggs;
   private boolean hasHighlight;
   private Keyword keyword;
   private List term;
   private List terms;
   private List ranges;
   private List sorts;
   private List aggs;
   private List highlights;

   public Criterias(CriteriasBuilder cb) {
      this.from = cb.getFrom();
      this.size = cb.getSize();
      this.hasFilter = cb.isHasFilter();
      this.hasSort = cb.isHasSort();
      this.hasAggs = cb.isHasAggs();
      this.hasHighlight = cb.isHasHighlight();
      this.keyword = cb.getKeyword();
      this.term = cb.getTerm();
      this.terms = cb.getTerms();
      this.ranges = cb.getRanges();
      this.sorts = cb.getSorts();
      this.aggs = cb.getAggs();
      this.highlights = cb.getHighlights();
   }

   public int getFrom() {
      return this.from;
   }

   public int getSize() {
      return this.size;
   }

   public boolean isHasFilter() {
      return this.hasFilter;
   }

   public boolean isHasSort() {
      return this.hasSort;
   }

   public boolean isHasAggs() {
      return this.hasAggs;
   }

   public boolean isHasHighlight() {
      return this.hasHighlight;
   }

   public Keyword getKeyword() {
      return this.keyword;
   }

   public List getTerm() {
      return this.term;
   }

   public List getTerms() {
      return this.terms;
   }

   public List getRanges() {
      return this.ranges;
   }

   public List getSorts() {
      return this.sorts;
   }

   public List getAggs() {
      return this.aggs;
   }

   public List getHighlights() {
      return this.highlights;
   }
}
