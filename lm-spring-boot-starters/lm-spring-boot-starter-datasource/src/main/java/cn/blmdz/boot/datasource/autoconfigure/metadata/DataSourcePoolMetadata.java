package cn.blmdz.boot.datasource.autoconfigure.metadata;

public interface DataSourcePoolMetadata {
   Float getUsage();

   Integer getActive();

   Integer getMax();

   Integer getMin();

   String getValidationQuery();
}
