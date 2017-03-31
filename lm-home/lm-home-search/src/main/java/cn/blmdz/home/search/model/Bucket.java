package cn.blmdz.home.search.model;

import java.io.Serializable;

import lombok.Data;

@Data
public class Bucket implements Serializable {
   private static final long serialVersionUID = 4330763248164026237L;
   private String key;
   private Long doc_count;
}
