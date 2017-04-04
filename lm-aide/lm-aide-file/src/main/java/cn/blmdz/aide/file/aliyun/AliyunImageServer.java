package cn.blmdz.aide.file.aliyun;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.multipart.MultipartFile;

import com.aliyun.oss.OSSClient;
import com.aliyun.oss.model.ObjectMetadata;
import com.google.common.base.Throwables;
import com.google.common.io.Files;

import cn.blmdz.aide.file.ImageServer;
import cn.blmdz.aide.file.exception.FileException;
import cn.blmdz.aide.file.exception.ImageException;
import cn.blmdz.aide.file.util.FUtil;

public class AliyunImageServer implements ImageServer {
	private static final Logger log = LoggerFactory.getLogger(AliyunImageServer.class);
	private final String bucketName;
	private final OSSClient ossClient;

	public AliyunImageServer(String endpoint, String appKey, String appSecret, String bucketName) {
		this.bucketName = bucketName;
		this.ossClient = new OSSClient(endpoint, appKey, appSecret);
	}

	public String write(String path, MultipartFile image) throws ImageException {
		try {
			ObjectMetadata metadata = new ObjectMetadata();
			metadata.setContentType(FUtil.contentType(path));
			metadata.setContentLength(image.getSize());
			InputStream stream = image.getInputStream();
			Throwable var5 = null;

			try {
				this.ossClient.putObject(this.bucketName, FUtil.upPath(path), stream, metadata);
			} catch (Throwable var15) {
				var5 = var15;
				throw var15;
			} finally {
				if (stream != null) {
					if (var5 != null) {
						try {
							stream.close();
						} catch (Throwable var14) {
							var5.addSuppressed(var14);
						}
					} else {
						stream.close();
					}
				}

			}

			return path;
		} catch (Exception var17) {
			log.error("failed to upload image(path={}) to oss, cause:{}", path,
					Throwables.getStackTraceAsString(var17));
			throw new ImageException(var17);
		}
	}

	public String write(String path, File image) throws ImageException {
		try {
			ObjectMetadata metadata = new ObjectMetadata();
			metadata.setContentType(FUtil.contentType(path));
			metadata.setContentLength(image.length());
			InputStream stream = Files.asByteSource(image).openStream();
			Throwable var5 = null;

			try {
				this.ossClient.putObject(this.bucketName, FUtil.upPath(path), stream, metadata);
			} catch (Throwable var15) {
				var5 = var15;
				throw var15;
			} finally {
				if (stream != null) {
					if (var5 != null) {
						try {
							stream.close();
						} catch (Throwable var14) {
							var5.addSuppressed(var14);
						}
					} else {
						stream.close();
					}
				}

			}

			return path;
		} catch (Exception var17) {
			log.error("failed to upload image(path={}) to oss, cause:{}", path,
					Throwables.getStackTraceAsString(var17));
			throw new ImageException(var17);
		}
	}

	public String write(String path, InputStream inputStream) throws FileException {
		String var4;
		try {
			ObjectMetadata metadata = new ObjectMetadata();
			metadata.setContentType(FUtil.contentType(path));
			metadata.setContentLength((long) FUtil.file(inputStream).getSize().intValue());
			this.ossClient.putObject(this.bucketName, FUtil.upPath(path), inputStream, metadata);
			var4 = path;
		} catch (Exception var13) {
			log.error("failed to upload file(path={}) to oss, cause:{}", path, Throwables.getStackTraceAsString(var13));
			throw new FileException(var13);
		} finally {
			if (inputStream != null) {
				try {
					inputStream.close();
				} catch (IOException var12) {
					log.error("Close file inputStream failed, path={}, error code={}", path,
							Throwables.getStackTraceAsString(var12));
				}
			}

		}

		return var4;
	}

	public boolean delete(String path) throws ImageException {
		try {
			this.ossClient.deleteObject(this.bucketName, FUtil.upPath(path));
			return true;
		} catch (Exception var3) {
			log.error("failed to delete {} from oss, cause:{}", path, Throwables.getStackTraceAsString(var3));
			throw new ImageException(var3);
		}
	}
}
