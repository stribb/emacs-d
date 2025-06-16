;; gemini-models.el --- This file defines the data for gptel--gemini-models -*- lexical-binding: t -*-

;;; Commentary:
;; (defvar gptel--gemini-models
;;   "gptel--gemini-models is a variable defined in ‘gptel-gemini.el’.
;;
;; Its value is shown below.
;;
;; List of available Gemini models and associated properties.
;;
;; Keys:
;;
;; - ‘:description’: a brief description of the model.
;;
;; - ‘:capabilities’: a list of capabilities supported by the model.
;;
;; - ‘:mime-types’: a list of supported MIME types for media files.
;;
;; - ‘:context-window’: the context window size, in thousands of tokens.
;;
;; - ‘:input-cost’: the input cost, in US dollars per million tokens.
;;
;; - ‘:output-cost’: the output cost, in US dollars per million tokens.
;;
;; - ‘:cutoff-date’: the knowledge cutoff date.
;;
;; - ‘:request-params’: a plist of additional request parameters to
;;   include when using this model.
;;
;; Information about the Gemini models was obtained from the following
;; source:
;;
;; - <https://ai.google.dev/pricing>
;; - <https://cloud.google.com/vertex-ai/generative-ai/docs/learn/models>
;; - <https://ai.google.dev/gemini-api/docs/models>
;;
;; This variable may be risky if used as a file-local variable."
;;   '((gemini-1.5-pro-latest :description "Google's latest model with enhanced capabilities across various tasks"
;;                            :capabilities (tool-use json media)
;;                            :mime-types ("image/png" "image/jpeg" "image/webp"
;;                                         "image/heic" "image/heif" "application/pdf"
;;                                         "text/plain" "text/csv" "text/html")
;;                            :context-window 2000
;;                            :input-cost 2.5
;;                            :output-cost 10
;;                            :cutoff-date "2024-05")
;;     (gemini-2.0-flash-exp :description "Next generation features, superior speed, native tool use"
;;                           :capabilities (tool-use json media)
;;                           :mime-types ("image/png" "image/jpeg" "image/webp"
;;                                        "image/heic" "image/heif" "application/pdf"
;;                                        "text/plain" "text/csv" "text/html")
;;                           :context-window 1000
;;                           :cutoff-date "2024-12")))

;;; Code:
(require 'gptel)

(setq gptel--gemini-models
      '((gemini-2.5-pro-preview-05-06
         :description "Most powerful Gemini thinking model..."
         :capabilities (tool-use json media audio video)
         :mime-types   ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                        "application/pdf" "text/plain" "text/csv" "text/html"
                        "audio/mpeg" "audio/wav" "audio/ogg" "audio/flac" "audio/aac" "audio/mp3"
                        "video/mp4" "video/mpeg" "video/avi" "video/quicktime" "video/webm")
         :context-window 1024
         :input-cost 1.25
         :output-cost 10.0
         :cutoff-date "2025-01")

        (gemini-2.5-flash-preview-05-20
         :description "Our best model in terms of price-performance..."
         :capabilities (tool-use json media audio video)
         :mime-types   ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                        "application/pdf" "text/plain" "text/csv" "text/html"
                        "audio/mpeg" "audio/wav" "audio/ogg" "audio/flac" "audio/aac" "audio/mp3"
                        "video/mp4" "video/mpeg" "video/avi" "video/quicktime" "video/webm")
         :context-window 1024
         :input-cost 0.15
         :output-cost 0.60
         :cutoff-date "2025-01")

        (gemini-2.0-flash-lite
         :description "Fastest and most cost-efficient Flash model..."
         :capabilities (json media audio video)
         :mime-types   ("image/png" "image/jpeg" "image/webp"
                        "application/pdf" "text/plain"
                        "audio/x-aac" "audio/flac" "audio/mp3" "audio/m4a" "audio/mpeg" "audio/mpga" "audio/mp4" "audio/opus" "audio/pcm" "audio/wav" "audio/webm"
                        "video/x-flv" "video/quicktime" "video/mpeg" "video/mp4" "video/webm" "video/wmv" "video/3gpp")
         :context-window 1024
         :input-cost 0.075
         :output-cost 0.30
         :cutoff-date "2024-06")

        (gemma-3-27b-it
         :description "Gemma 3: Largest instruction-tuned open model..."
         :capabilities (media json audio)
         :mime-types   ("image/png" "image/jpeg" "image/webp"
                        "text/plain" "application/pdf"
                        "audio/mpeg" "audio/wav" "audio/ogg" "audio/flac")
         :context-window 128
         :input-cost 0.0
         :output-cost 0.0
         :cutoff-date "2024-08")))

(length gptel--gemini-models)

(provide 'gemini-models)

;;; gemini-models.el ends here
